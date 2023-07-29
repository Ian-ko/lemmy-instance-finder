dataFields={}
function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end
function writeData(dataFields)
   local f = io.open("ReadyData.txt", "w")
   for i=1,#dataFields do
     name = dataFields[i][1]
     url = dataFields[i][2]
     description = dataFields[i][3]
     io.output(f)
     io.write(name.."\n"..url.."\n"..description.."\n")
   end
   io.close(f)
end
if file_exists("ReadyData.txt") then
  print("File ReadyData.txt already exists, refusing to overwrite")
  error("refused to continue")
else
  while true do
    print("Enter url or type STOP")
    url=io.read()
    if url == "STOP" then
      break
    end
    print("Enter description")
    dataFields[#dataFields+1] = {string.sub(url,9,string.len(url)),url,io.read()}
  end
  myfile=writeData(dataFields)
end
