import Command
import RCommand

data Device = Device { async :: String -> IO () }

send :: Device -> Command -> IO ()
send d m = async d (show m)

device :: Device
device = Device (execRCommand . read)

main = send device (Say "Hello")
