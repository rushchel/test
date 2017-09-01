using System;
using System.Data;
using System.Web;
using System.Web.Services;
using System.Web.Services.Protocols;
using WS.Ekassir;
using Oracle.DataAccess.Client;
using Oracle.DataAccess.Types;
using AutoMapper;
using System.Collections.Generic;
using System.Linq;

namespace WS
{
    [WebService(Description = "Integretion agent", Namespace = XmlNS)]
    [WebServiceBinding(ConformsTo = WsiProfiles.BasicProfile1_1)]
	public class Soap : System.Web.Services.WebService
	{
		public const string XmlNS = "http://localhost/ia/";
        public const string oradb = "Data Source=XE;User ID=RUSH;Password=rush";
        public class Person
        {
            public string FirstName { get; set; }
            public string LastName { get; set; }
            public int Age { get; set; }
        }

        public sealed class SimplePersonProfile : Profile
        {
            private void Configure()
            {
                IMappingExpression<DataRow, Person> mappingExpression;

                mappingExpression = CreateMap<DataRow, Person>();
                mappingExpression.ForMember(d => d.FirstName, o => o.MapFrom(s => s["FirstName"]));
                mappingExpression.ForMember(d => d.LastName, o => o.MapFrom(s => s["LastName"]));
                mappingExpression.ForMember(d => d.Age, o => o.MapFrom(s => s["Age"]));

                return;
            }

            public SimplePersonProfile()
            {
                Configure();
            }
        }

        [WebMethod(Description = "Person")]
        public List<Person> GetPerson()
        {
            var dt = new DataTable();
            dt.Columns.Add("FirstName", typeof(string));
            dt.Columns.Add("LastName", typeof(string));
            dt.Columns.Add("Age", typeof(int));

            var dr = dt.NewRow();
            dr["FirstName"] = "Person";
            dr["LastName"] = "One";
            dr["Age"] = 23;
            dt.Rows.Add(dr);

            dr = dt.NewRow();
            dr["FirstName"] = "Person";
            dr["LastName"] = "Two";
            dr["Age"] = 43;
            dt.Rows.Add(dr);

            /*Mapper.Initialize(cfg =>
            {
                cfg.CreateMap<IDataReader, List<Person>>();
                cfg.CreateMap<IDataReader, IEnumerable<Person>>();
            });
            List<Person> people = Mapper.Map<IDataReader, List<Person>>(dt.CreateDataReader());*/

            MapperConfiguration configuration;
            configuration = new MapperConfiguration(a =>
            {
                a.AddProfile(new SimplePersonProfile());
            });
            IMapper mapper = configuration.CreateMapper();
            List<DataRow> list = dt.AsEnumerable().ToList();
            List<Person> people = mapper.Map<List<DataRow>, List<Person>>(list);
            return people;
        }

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
                    return ds.Tables["Client"].Rows[0]["Name"].ToString();
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
                    client.Id = Convert.ToInt32(ds.Tables["Client"].Rows[0]["Id"]);
                    client.Name = ds.Tables["Client"].Rows[0]["Name"].ToString();
                    client.Blocked = (ds.Tables["Client"].Rows[0]["Blocked"].ToString() == "1") ? true : false;

                    //List<Client> clients = AutoMapper.Mapper.Map<IDataReader, List<Client>>(ds.Tables["Client"].CreateDataReader());
                    //client.Id = clients[1].Id;
                    requisites.LegalName = "АО ОТП БАНК";
			        requisites.LegalAddress = "ул. Орджоникидзе 3А";
			        requisites.PostAddress = "г. Омск - 644043, ул. Орджоникидзе 3А";
			        requisites.TaxpayerIdNumber = "550777777777";
			        requisites.RRCode = "550999999999";
			        requisites.ContactPhoneNumber = "+73812123456";
			        client.Requisites = requisites;
			        response.Client = client;
                    return response;
                }
                catch (Exception ex)
                {
                    ErrorResponse err = new ErrorResponse();
                    err.Error.Description = ex.Message;
                    return err;
                }
            }
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
