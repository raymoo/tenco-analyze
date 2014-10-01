# coding: utf-8

require 'rubygems'
require 'nkf'
require 'net/http'
Net::HTTP.version_1_2
require 'rexml/document'
require 'time'
require 'digest/sha1'
require 'optparse'

require './lib/tenco_reporter/config_util'
include TencoReporter::ConfigUtil
require './lib/tenco_reporter/track_record_util'
include TencoReporter::TrackRecordUtil
require './lib/tenco_reporter/stdout_to_cp932_converter'

# プログラム情報
PROGRAM_VERSION = '0.04'
PROGRAM_NAME = 'Tensokukan Reporter Tool'

# 設定
TRACKRECORD_POST_SIZE = 250   # 一度に送信する対戦結果数
DUPLICATION_LIMIT_TIME_SECONDS = 2   # タイムスタンプが何秒以内のデータを、重複データとみなすか
ACCOUNT_NAME_REGEX = /\A[a-zA-Z0-9_]{1,32}\z/
MAIL_ADDRESS_REGEX = /\A[\x01-\x7F]+@(([-a-z0-9]+\.)*[a-z]+|\[\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}\])\z/ # メールアドレスチェック用正規表現
PLEASE_RETRY_FORCE_INSERT = "<Please Retry in Force-Insert Mode>"  # 強制インサートリトライのお願い文字列
HTTP_REQUEST_HEADER = {"User-Agent" => "Tensokukan Report Tool #{PROGRAM_VERSION}"}
RECORD_SW_NAME = 'Tensokukan' # 対戦記録ソフトウェア名
DB_TR_TABLE_NAME = 'trackrecord123' # DBの対戦結果テーブル名
WEB_SERVICE_NAME = 'Tenco!'  # サーバ側のサービス名

# デフォルト値
DEFAULT_GAME_ID = 2    # ゲームID
DEFAULT_DATABASE_FILE_PATH = '../*.db' # データベースファイルパス

# ログファイルパス
ERROR_LOG_PATH = 'error.txt'

# 送信設定
is_force_insert = false # 強制インサートモード。はじめは false。
is_all_report = false # 全件報告モード。サーバーからの最終対戦時刻をとらず、全件送信。

# 変数
latest_version = nil # クライアントの最新バージョン
trackrecord = [] # 対戦結果
is_read_trackrecord_warning = false # 対戦結果読み込み時に警告があったかどうか
is_warning_exist = false # 警告メッセージがあるかどうか

puts "*** #{PROGRAM_NAME} ***"
puts "ver.#{PROGRAM_VERSION}\n\n"

begin

  ### 設定読み込み ###

  # 設定ファイルパス
  config_file = 'config.yaml'
  config_default_file = 'config_default.yaml'
  env_file = 'env.yaml'

  # 設定ファイルがなければデフォルトをコピーして作成
  unless File.exist?(config_file) then
    open(config_default_file) do |s|
      open(config_file, "w") do |d|
        d.write(s.read)
      end
    end
  end

  # サーバー環境設定ファイルがなければ、エラー終了
  unless File.exist?(env_file) then
    raise "#{env_file} could not be found.\nPlease copy it from the folder you downloaded it with."
  end

  # 設定ファイル読み込み
  config = load_config(config_file) 
  env    = load_config(env_file)
      
  # config.yaml がおかしいと代入時にエラーが出ることに対する格好悪い対策
  config ||= {}
  config['account'] ||= {}
  config['database'] ||= {}

  account_name = config['account']['name'].to_s || ''
  account_password = config['account']['password'].to_s || ''

  # ゲームIDを設定ファイルから読み込む機能は -g オプションが必要
  game_id = DEFAULT_GAME_ID
  db_file_path = config['database']['file_path'].to_s || DEFAULT_DATABASE_FILE_PATH

  # proxy_host = config['proxy']['host']
  # proxy_port = config['proxy']['port']
  # last_report_time = config['last_report_time']
  # IS_USE_HTTPS = false

  SERVER_TRACK_RECORD_HOST = env['server']['track_record']['host'].to_s
  SERVER_TRACK_RECORD_PATH = env['server']['track_record']['path'].to_s
  SERVER_LAST_TRACK_RECORD_HOST = env['server']['last_track_record']['host'].to_s
  SERVER_LAST_TRACK_RECORD_PATH = env['server']['last_track_record']['path'].to_s
  SERVER_ACCOUNT_HOST = env['server']['account']['host'].to_s
  SERVER_ACCOUNT_PATH = env['server']['account']['path'].to_s
  CLIENT_LATEST_VERSION_HOST = env['client']['latest_version']['host'].to_s
  CLIENT_LATEST_VERSION_PATH = env['client']['latest_version']['path'].to_s
  CLIENT_SITE_URL = "http://#{env['client']['site']['host']}#{env['client']['site']['path']}"

  ### クライアント最新バージョンチェック ###

  # puts "★クライアント最新バージョン自動チェック"
  # puts 
  
  def get_latest_version(latest_version_host, latest_version_path)
    response = nil
    Net::HTTP.new(latest_version_host, 80).start do |s|
      response = s.get(latest_version_path, HTTP_REQUEST_HEADER)
    end  
    response.code == '200' ? response.body.strip : nil
  end
  
  begin
    latest_version = get_latest_version(CLIENT_LATEST_VERSION_HOST, CLIENT_LATEST_VERSION_PATH)
    
    case
    when latest_version.nil?
      # puts "！最新バージョンの取得に失敗しました。"
      # puts "スキップして続行します。"
    when latest_version > PROGRAM_VERSION then
      puts "★A new version of #{PROGRAM_NAME} is available.（ver.#{latest_version}）"
      puts "Would you like to see it in your browser? (Press N to skip)"
      print "> "
      case gets[0..0]
      when "N" then
        puts "Skipping."
        puts 
      else
        system "start #{CLIENT_SITE_URL}"
        exit
      end
    when latest_version <= PROGRAM_VERSION then
      # puts "お使いのバージョンは最新です。"
      # puts 
    end
    
  rescue => ex
    puts "The client's newest version check has resulted in an error."
    puts ex.to_s
    # puts ex.backtrace.join("\n")
    puts ex.class
    puts
    puts "Skipping."
    puts
  end
    
  ### メイン処理 ###

  ## オプション設定
  opt = OptionParser.new

  opt.on('-a') {|v| is_all_report = true} # 全件報告モード

  # 設定ファイルのゲームID設定を有効にする
  opt.on('-g') do |v|
    begin
      game_id = config['game']['id'].to_i
    rescue => ex
      raise "Error: The game ID could not be found from the setting file (#{config_file})."
    end
    
    if game_id.nil? || game_id < 1 then
      raise "Error: The game id recorded in the setting file (#{config_file}) is malformed."
    end
    
    puts "★Reporting with the config file's game id (#{game_id})"
  end

  # 設定ファイルのデータベースファイルパスをデフォルトに戻す
  opt.on('--database-filepath-default-overwrite') do |v|  
    puts "★Overwriting the configured DB path at #{RECORD_SW_NAME}"  
    puts "Overwriting #{config_file}'s DB file path at #{RECORD_SW_NAME} with #{DEFAULT_DATABASE_FILE_PATH}"
    config['database']['file_path'] = DEFAULT_DATABASE_FILE_PATH
    save_config(config_file, config)
    puts "Saved settings!"
    puts
    exit
  end

  opt.parse!(ARGV)

  ## アカウント設定（新規アカウント登録／既存アカウント設定）処理
  unless (account_name && account_name =~ ACCOUNT_NAME_REGEX) then
    is_new_account = nil
    account_name = ''
    account_password = ''
    is_account_register_finish = false
    
    puts "★#{WEB_SERVICE_NAME} Account Setting (First Time)\n"  
    puts "If you are using #{WEB_SERVICE_NAME} for the first time, press \"1\" and hit Enter.\n"  
    puts "If you already have an account with one of the reporters, hit \"2\" and hit Enter.\n"  
    puts
    print "> "
    
    while (input = gets)
      input.strip!
      if input == "1"
        is_new_account = true
        puts
        break
      elsif input == "2"
        is_new_account = false
        puts
        break
      end
      puts 
      puts "★#{WEB_SERVICE_NAME} Account Setting (First Time)\n"  
      puts "If you are using #{WEB_SERVICE_NAME} for the first time, press \"1\" and hit Enter.\n"  
      puts "If you already have an account with one of the reporters, hit \"2\" and hit Enter.\n"  
      puts
      print "> "
    end
    
    if is_new_account then
      
      puts "★New #{WEB_SERVICE_NAME} account registration\n\n"  
      
      while (!is_account_register_finish)
        # アカウント名入力
        puts "Enter your desired username.\n"  
        puts "Your username will become part of your profile URL.\n"  
        puts "(Halfwidth alphanumeric characters and underscores only. Max 32 characters)\n"
        print "Username> "  
        while (input = gets)
          input.strip!
          if input =~ ACCOUNT_NAME_REGEX then
            account_name = input
            puts 
            break
          else
            puts "！Please only use half-width alphanumeric characters or underscores, and keep it under 32 characters.\n"
            print "Username> "  
          end
        end
        
        # パスワード入力
        puts "Enter a password (No restrictions on characters. Must be within 4 and 16 bytes. Cannot be the same as your username.\n"
        print "Password> "  
        while (input = gets)
          input.strip!
          if (input.length >= 4 and input.length <= 16 and input != account_name) then
            account_password = input
            break
          else
            puts "！Please keep your password between 4 and 16 bytes, and do not use your username."
            print "Password> "  
          end
        end 
        
        print "Password (Confirm)> "  
        while (input = gets)
          input.strip!
          if (account_password == input) then
            puts 
            break
          else
            puts "！The passwords do not match.\n"  
            print "Passoword (Confirm)> "  
          end
        end
        
        # メールアドレス入力
        puts "Enter your email address (Optional)\n"  
        puts "※This will be used to contact you if you forget your password.\n"  
        puts "※If you do not enter anything, we will not be able to send you your password.\n"  
        print "Email> "
        while (input = gets)
          input.strip!
          if (input == '') then
            account_mail_address = ''
            puts "Not registering email address."  
            puts
            break
          elsif input =~ MAIL_ADDRESS_REGEX and input.length <= 256 then
            account_mail_address = input
            puts
            break
          else
            puts "！Please format your address correctly and keep it within 256 bytes."
            print "Email> "
          end
        end
        
        # 新規アカウントをサーバーに登録
        puts "Registering your account with the server...\n"
        
        # アカウント XML 生成
        account_xml = REXML::Document.new
        account_xml << REXML::XMLDecl.new('1.0', 'UTF-8')
        account_element = account_xml.add_element("account")
        account_element.add_element('name').add_text(account_name)
        account_element.add_element('password').add_text(account_password)
        account_element.add_element('mail_address').add_text(account_mail_address)
        # サーバーに送信
        response = nil
        # http = Net::HTTP.new(SERVER_ACCOUNT_HOST, 443)
        # http.use_ssl = true
        # http.verify_mode = OpenSSL::SSL::VERIFY_NONE
        http = Net::HTTP.new(SERVER_ACCOUNT_HOST, 80)
        http.start do |s|
          response = s.post(SERVER_ACCOUNT_PATH, account_xml.to_s, HTTP_REQUEST_HEADER)
        end
        
        print "Server Response:\n"  
        response.body.each_line do |line|
          puts "> #{line}"
        end

        if response.code == '200' then
        # アカウント登録成功時
          is_account_register_finish = true
          config['account']['name'] = account_name
          config['account']['password'] = account_password
          
          save_config(config_file, config)
          
          puts 
          puts "Saved account info to config file."
          puts "Please check the server response."
          puts
          puts "Press Enter to continue with reporting your match log."
          gets
          
          puts "Reporting log..."
          puts
        else
        # アカウント登録失敗時
          puts "Retrying account registration...\n\n"
          sleep 1
        end
        
      end # while (!is_account_register_finish)
    else

      puts "★Editing configuration file\n"
      puts "Set your #{WEB_SERVICE_NAME} username and password."
      puts "※If you do not know your username or password, you can check it in a previously-used #{WEB_SERVICE_NAME} client's (e.g. Hikouseki Reporter Tool) #{config_file}"
      puts 
      puts "Input your #{WEB_SERVICE_NAME} username."
      
      # アカウント名入力
      print "Username> "
      while (input = gets)
        input.strip!
        if input =~ ACCOUNT_NAME_REGEX then
          account_name = input
          puts 
          break
        else
          puts "！Please only use half-width alphanumeric characters or underscores, and keep it under 32 characters."
        end
        print "Username> "
      end
      
      # パスワード入力
      puts "Enter your password.\n"
      print "Password> "
      while (input = gets)
        input.strip!
        if (input.length >= 4 and input.length <= 16 and input != account_name) then
          account_password = input
          puts
          break
        else
          puts "！Please enter a password between 4 and 16 bytes different from your username."
        end
        print "Password> "
      end
      
      # 設定ファイル保存
      config['account']['name'] = account_name
      config['account']['password'] = account_password
      save_config(config_file, config)
      
      puts "Saved account info to config file.\n\n"
      puts "Reporting log...\n\n"
      
    end # if is_new_account
    
    sleep 2

  end

    
  ## 登録済みの最終対戦結果時刻を取得する
  unless is_all_report then
    puts "★Retrieving the time of your last reported match..."
    puts "GET http://#{SERVER_LAST_TRACK_RECORD_HOST}#{SERVER_LAST_TRACK_RECORD_PATH}?game_id=#{game_id}&account_name=#{account_name}"

    http = Net::HTTP.new(SERVER_LAST_TRACK_RECORD_HOST, 80)
    response = nil
    http.start do |s|
      response = s.get("#{SERVER_LAST_TRACK_RECORD_PATH}?game_id=#{game_id}&account_name=#{account_name}", HTTP_REQUEST_HEADER)
    end

    if response.code == '200' or response.code == '204' then
      if (response.body and response.body != '') then
        last_report_time = Time.parse(response.body)
        puts "Time of last match reported: #{last_report_time.strftime('%Y/%m/%d %H:%M:%S')}"
      else
        last_report_time = Time.at(0)
        puts "No matches recorded on server."
      end
    else
      raise "An error occured while getting the time of your last reported match. Halting."
    end
  else
    puts "★You are set to report all matches. Skipping retrieval of last report time."
    last_report_time = Time.at(0)
  end
  puts

  ## 対戦結果報告処理
  puts "★Transmitting Match Log"
  puts "Reporting matches since " + last_report_time.strftime('%Y/%m/%d %H:%M:%S') + " from #{RECORD_SW_NAME}"
  puts

  # DBから対戦結果を取得
  db_files = Dir::glob(NKF.nkf('-Wsxm0 --cp932', db_file_path))

  if db_files.length > 0
    trackrecord, is_read_trackrecord_warning = read_trackrecord(db_files, last_report_time + 1)
    is_warning_exist = true if is_read_trackrecord_warning
  else
    raise <<-MSG
  The #{RECORD_SW_NAME} database file set in #{config_file} was not found
・Please check if #{PROGRAM_NAME} is installed correctly.
  If you are using the default settings, put the #{PROGRAM_NAME} folder in the #{RECORD_SW_NAME} folder.
・If you have modified #{config_file}, check if the settings are correct.
    MSG
  end

  puts

  ## 報告対象データの送信処理

  # 報告対象データが0件なら送信しない
  if trackrecord.length == 0 then
    puts "No data to report."
  else
    
    # 対戦結果データを分割して送信
    0.step(trackrecord.length, TRACKRECORD_POST_SIZE) do |start_row_num|
      end_row_num = [start_row_num + TRACKRECORD_POST_SIZE - 1, trackrecord.length - 1].min
      response = nil # サーバーからのレスポンスデータ
      
      puts "Out of #{trackrecord.length}items, reporting items #{start_row_num + 1}～#{end_row_num + 1} #{is_force_insert ? "(Force Insert Mode" : ""}...\n"
      
      # 送信用XML生成
      trackrecord_xml_string = trackrecord2xml_string(game_id, account_name, account_password, trackrecord[start_row_num..end_row_num], is_force_insert)
      File.open('./last_report_trackrecord.xml', 'w') do |w|
        w.puts trackrecord_xml_string
      end

      # データ送信
      # https = Net::HTTP.new(SERVER_TRACK_RECORD_HOST, 443)
      # https.use_ssl = true
      # https.verify_mode = OpenSSL::SSL::VERIFY_NONE
      # https = Net::HTTP::Proxy(proxy_addr, proxy_port).new(SERVER_TRACK_RECORD_HOST,443)
      # https.ca_file = '/usr/share/ssl/cert.pem'
      # https.verify_depth = 5
      # https.verify_mode = OpenSSL::SSL::VERIFY_PEER
      http = Net::HTTP.new(SERVER_TRACK_RECORD_HOST, 80)
      http.start do |s|
        response = s.post(SERVER_TRACK_RECORD_PATH, trackrecord_xml_string, HTTP_REQUEST_HEADER)
      end
      
      # 送信結果表示
      puts "Server response:"
      response.body.each_line do |line|
        puts "> #{line}"
      end
      puts
      
      if response.code == '200' then
        sleep 1
        # 特に表示しない
      else
        if response.body.index(PLEASE_RETRY_FORCE_INSERT)
          puts "Retrying report with force-insert mode. Reporting in 5 seconds...\n\n"
          sleep 5
          is_force_insert = true
          redo
        else
          raise "A server error occured during the report. Halting..."
        end
      end
    end
  end

  # 設定ファイル更新
  save_config(config_file, config)
      
  puts

  # 終了メッセージ出力
  if is_warning_exist then
    puts "The reporting completed successfully, but the server sent a notice."
    puts "Please check the output."
    puts
    puts "Press Enter to finish."
    exit if gets
    puts
  else
    puts "Reporting executed successfully."
  end

  sleep 3

### 全体エラー処理 ###
rescue => ex
  if config && config['account'] then
    config['account']['name']     = '<secret>' if config['account']['name']
    config['account']['password'] = '<secret>' if config['account']['password']
  end
  
  puts 
  puts "An error occured during execution. Halting.\n"
  puts 
  puts '### Error details ###'
  puts
  puts ex.to_s
  puts
  puts ex.backtrace.join("\n")
  puts (config ? config.to_yaml : "Your config isn't set.")
  if response then
    puts
    puts "<Last message from the server>"
    puts "HTTP status code : #{response.code}"
    puts response.body
  end
  puts
  puts '### End Error Details ###'
  
  File.open(ERROR_LOG_PATH, 'a') do |log|
    log.puts "#{Time.now.strftime('%Y/%m/%d %H:%M:%S')} #{File::basename(__FILE__)} #{PROGRAM_VERSION}" 
    log.puts ex.to_s
    log.puts ex.backtrace.join("\n")
    log.puts config ? config.to_yaml : "No config set."
    if response then
      log.puts "<Last message from server>"
      log.puts "HTTP status code : #{response.code}"
      log.puts response.body
    end
    log.puts '********'
  end
  
  puts
  puts "The above error has been recorded in #{ERROR_LOG_PATH}"
  puts
  
  puts "Press Enter to finish"
  exit if gets
end
