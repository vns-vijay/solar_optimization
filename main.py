import pulp
import pandas as pd


class PreprocessData:
    def __init__(self, df):
        self.df = df
    

class ORModel:
    def __init__(self, data_dict):
        self.data_dict = data_dict
        self.timestamp = list(self.data_dict['avg_demand_w'].keys())
        self.c_b = self.data_dict['battery_cost']
        self.c_p = self.data_dict['pannel_cost']
        self.c_t = self.data_dict['avg_tarrif_kwh']
        self.demand = self.data_dict['avg_demand_w']
        self.solar_supply = self.data_dict['avg_solar_w']
        self.delta = 0.5
        self.dis_limit = 500
        self.charg_limit = 500


    def create_variables(self):
        
        self.y_b = pulp.LpVariable('y_b', lowBound=0, cat=pulp.LpInteger)
        self.y_s = pulp.LpVariable('y_s', lowBound=0, cat=pulp.LpInteger)

        self.x_c = {k: pulp.LpVariable('x_c'+str(k), lowBound=0, cat=pulp.LpContinuous) for k in self.timestamp}
        self.x_d = {k: pulp.LpVariable('x_d'+str(k), lowBound=0, cat=pulp.LpContinuous) for k in self.timestamp}
        self.s_c = {k: pulp.LpVariable('s_c'+str(k), lowBound=0, cat=pulp.LpContinuous) for k in self.timestamp}
        self.s_d = {k: pulp.LpVariable('s_d'+str(k), lowBound=0, cat=pulp.LpContinuous) for k in self.timestamp}

        self.z = {k: pulp.LpVariable('z'+str(k), lowBound=0, cat=pulp.LpContinuous) for k in self.timestamp}

        self.i = {k: pulp.LpVariable('i'+str(k), lowBound=0, cat=pulp.LpContinuous) for k in self.timestamp}
        self.i[0] = 0


    def initalize_prob(self):
        self.prob = pulp.LpProblem('SolarProb', sense=pulp.LpMinimize)
    
    def get_objective(self):
        self.prob += self.y_b*self.c_b + self.y_s*self.c_p + pulp.lpSum(self.c_t[t]*(self.x_c[t]+self.x_d[t]) 
                                                                        for t in self.timestamp)

    def charging_const(self):
        for t in self.timestamp:
            self.prob += self.i[t-1] + self.delta*(self.x_c[t]+self.x_d[t]) - self.delta*self.z[t] == self.i[t]

    def discharging_limit(self):
        for t in self.timestamp:
            self.prob += self.z[t] <= self.dis_limit

    def charging_limit(self):
        for t in self.timestamp:
            self.prob += self.x_c[t]+self.s_c[t] <= self.charg_limit

    def demand_const(self):
        for t in self.timestamp:
            self.prob += self.s_d[t] + self.x_d[t] + self.z[t] == self.demand[t]

    def solar_supply_const(self):
        for t in self.timestamp:
            self.prob += self.s_c[t] + self.s_d[t] == self.solar_supply[t]

    def battery_capacity_const(self):
        for t in self.timestamp:
            self.prob += self.i[t] <= self.y_b
    
    def solve(self):
        self.create_variables()
        self.initalize_prob()
        self.get_objective()
        self.charging_const()
        self.discharging_limit()
        self.charging_limit()
        self.demand_const()
        self.battery_capacity_const()
        self.solar_supply_const()

        status = self.prob.solve()
        print("=========")
        print(status)
        print("=========")





df = pd.read_csv('/Users/vijaysharma/Downloads/argus_assn/input_data/Argus_Media_Optim_Test_Data.csv')
df['tariff_p_kwh'] = df['tariff_p_kwh']/100
df['timestamp'] = pd.to_datetime(df['timestamp'], format='%d/%m/%Y %H:%M')
df['time_part'] = df['timestamp'].dt.strftime('%H%M')
df['time_part'] = df['time_part'].astype(int).rank(method='dense').astype(int)


aggr_df = df.groupby('time_part').agg(avg_demand_w = ('demand_w', 'mean'),
                                    avg_solar_w = ('solar_w', 'mean'),
                                    avg_tarrif_kwh = ('tariff_p_kwh', 'mean')) #.reset_index()

aggr_df = aggr_df.round(2)
dct = aggr_df.to_dict(orient='dict')

params = {
    'battery_cost':0.5,   #Wh
    'pannel_cost':300,   # Euro
}
params.update(dct)
print(params)

ob = ORModel(params).solve()
