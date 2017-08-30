using System;
using System.Data;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;
using WS.Ekassir;
//using System.Data.OracleClient;
using Oracle.DataAccess.Client;
using Oracle.DataAccess.Types;

namespace WS
{
    [WebService(Description = "Integretion agent", Namespace = XmlNS)]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
	public class Soap : System.Web.Services.WebService
	{
		public const string XmlNS = "http://localhost/ia/";
        public const string oradb = "Data Source=XE;User ID=RUSH;Password=rush";

        [WebMethod(Description = "Plus")]
		public int WebAdd(int x, int y)
		{
			return x + y;
		}
		
		[WebMethod(Description = "Table request")]
		public string GetTable()
		{
			using (OracleConnection conn = new OracleConnection(oradb))
			{
                try
                {
                    OracleCommand cmd = new OracleCommand("CURSPKG.TWO_CURSORS", conn);
                    cmd.CommandType = CommandType.StoredProcedure;
                    cmd.Parameters.Add("INP_NUM", OracleDbType.Double, 100).Value = 123;
                    cmd.Parameters.Add("CURSOR_01", OracleDbType.RefCursor).Direction = ParameterDirection.Output;
                    cmd.Parameters.Add("CURSOR_02", OracleDbType.RefCursor).Direction = ParameterDirection.Output;

                    OracleDataAdapter da = new OracleDataAdapter(cmd);
                    da.TableMappings.Add("Table", "Client");
                    da.TableMappings.Add("Table1", "ClientRequisites");

                    DataSet ds = new DataSet();
                    da.Fill(ds);
                    string xmlDS = ds.GetXml();
                    return xmlDS;
                }
                catch (Exception ex)
                {
                    return ex.Message;
                }
			}
		}

        [WebMethod(Description = "Objects request")]
        public string GetObjects()
        {
            using (OracleConnection conn = new OracleConnection(oradb))
            {
                try
                {
                    OracleCommand cmd = new OracleCommand("CURSPKG.GET_OBJECTS", conn);
                    cmd.CommandType = CommandType.StoredProcedure;
                    cmd.Parameters.Add("INP_NUM", OracleDbType.Double, 100).Value = 123;
                    cmd.Parameters.Add("REC", OracleDbType.Object).Direction = ParameterDirection.Output;
                    cmd.Parameters.Add("TAB", OracleDbType.Object).Direction = ParameterDirection.Output;

                    OracleDataAdapter da = new OracleDataAdapter(cmd);
                    da.TableMappings.Add("Table", "Record");
                    da.TableMappings.Add("Table1", "Table");

                    DataSet ds = new DataSet();
                    da.Fill(ds);
                    string xmlDS = ds.GetXml();
                    return xmlDS;
                }
                catch (Exception ex)
                {
                    return ex.Message;
                }
            }
        }

        [WebMethod(Description = "Dictinary request")]
		public Response GetDirectory (GetDirectoryRequest request)
		{
			GetDirectoryResponse response = new GetDirectoryResponse();
			Client client = new Client();
			Requisites requisites = new Requisites();
			client.Id = 101;
			//client.Name = "OTPBANK";
			client.Name = this.Context.Request.Headers.Get("Content-Type");
			client.Blocked = false;
			requisites.LegalName = "АО ОТП БАНК";
			requisites.LegalAddress = "ул. Орджоникидзе 3А";
			requisites.PostAddress = "г. Омск - 644043, ул. Орджоникидзе 3А";
			requisites.TaxpayerIdNumber = "550777777777";
			requisites.RRCode = "550999999999";
			requisites.ContactPhoneNumber = "+73812123456";
			client.Requisites = requisites;
			response.Client = client;
			this.Context.Response.AppendHeader("eKassir-Version", "666");
			return response;
		}
			
		/// <summary>
		/// Logs into the web service
		/// </summary>
		/// <param name="userName">The User Name to login in as</param>
		/// <param name="password">User's password</param>
		/// <returns>True on successful login.</returns>
		[WebMethod(EnableSession=true)]
		public bool Login(string userName, string password)
		{
			//NOTE: There are better ways of doing authentication. This is just illustrates Session usage.
			UserName = userName;
			return true;
		}
		
		/// <summary>
		/// Logs out of the Session.
		/// </summary>
		[WebMethod(EnableSession=true)]
		public void Logout()
		{    
			Context.Session.Abandon();
		}
		
		/// <summary>
		/// UserName of the logged in user.
		/// </summary>
		private string UserName {
			get {return (string)Context.Session["User"];}
			set {Context.Session["User"] = value;}
		}
	}
}
