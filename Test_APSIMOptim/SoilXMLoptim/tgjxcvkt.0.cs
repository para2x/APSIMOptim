using System;

using ModelFramework;

using CSGeneral;

 

public class Script
{     
   [Link] Paddock MyPaddock; // Can be used to dynamically get access to simulation structure and variables\
   [Link] Paddock pad; // Can be used to dynamically get access to simulation structure and variables\
   [Input] DateTime Today; // Equates to the value of the current simulation date - value comes from CLOCK   
   [Input] double wf;      // Use the same water factor (SoilN)
   [Input] double[] oc; //Soil OC percent
   [Input] double[] ph; //Soil pH
   [Input] double[] nh4;
   [Input] double[] no3;
   [Input] double[] dlayer;
   [Input] double[] hum_c;
   [Input] double[] biom_c;
   [Input] double[] bd;
   [Input] double rain;
   [Input] double[] nit_tot;
   
   [Input] double[] no3_min; //Min NO3 in soil per layer
   [Input] double[] nh4_min; //Min NH4 in soil per layer
   double[] tf;      // Use the same temperature factor (SoilN)
  
   [Param] int biochar_added;      // in kg/ha
   //[Param] double frac_c_biochar;  // C in biochar
   [Param] double biochar_loss;    // usually there is a loss during application that might impact calculations
   [Param] double MRT1;             // mean residence time for the labile biochar pool
   [Param] double MRT2;             // mean residence time for the resistant biochar pool
   //[Param] double frac_labile;     // labile fraction of biochar; this depends on the biochar type - varies from 1 to 30%.
   [Param] double ef_biochar;      // efficiency of biochar retained in the system
   [Param] double fr_biochar_biom; // a small portion goes to BIOM (usually this is zero) and the rest goes to HUM
   //[Param] double biochar_cn;         // CN ratio of the new biochar pool
   [Param] string date_of_application; //The date biochar was adde
   [Param] double sand; // Sand percent of soil
   [Param] double clay; //Clay percent of soil 
   [Param] string decomp_switch; //Whether or not biochar affects decomposition  
   [Param] string nitrification_switch; //Whether or not biochar affects nitrification
   [Param] string ph_switch; //whether biochar affects ph
   [Param] string ll_switch;
   [Param] string dul_switch;
   [Param] string xf_switch;
   [Param] string kl_switch;
   [Param] string bd_switch;
   [Param] string biochar_c_switch;
   [Param] string swcon_switch;
   [Param] string ks_switch;
   [Param] string sat_switch;
   [Param] double prim_hum;//priming effects
   [Param] double prim_biom;
   [Param] double prim_cell;
   [Param] double prim_carb;
   [Param] double prim_lign;
   [Param] double prim_fr;
   [Param] double prim_ef;
   [Param] double prim_fr_fom;
   [Param] double prim_ef_fom;
   [Param] double soil_cn; //C:N ratio of soil (SoilN)
   [Param] double biom_cn;
   [Param] double incorp_depth;
   [Param] double tillage;
   [Param] double dul_qual;
   [Param] double bd_qual;
   [Param] string soil_order;
   [Param] double nclay_portion;
   //[Param] double bc_cce;
   //[Param] double bc_cec;
   [Param] double cnrf_bc_coeff;
   [Param] double cnrf_bc_optcn;
   [Param] double uph;
   [Param] double lph;
   [Param] double ab_val;
   [Param] double bot_slope;
   [Param] double bc_wfps_factor;
   [Param] double nh4_adsorption;
   [Param] double nh4_desorption;
   //------------------------------------Params for GLMs
   [Param] string feedbio;
   [Param] double PTemp;
   
   

   [Output] double[] BiocharC;             // BiocharC = biochar_added * frac_c_biochar * biochar_loss
   [Output] double BiocharC_total;    // Ammount of biochar c in each individual soil layer
   [Output] double[] dlt_c_min_biochar;    // here I use a douple exponential function instead of a first order decay
   [Output] double[] dlt_c_biochar_co2;    // 1 - efficiency
   [Output] double[] dlt_c_biochar_biom;   //
   [Output] double[] dlt_c_biochar_hum;
   [Output] double[] dlt_n_min_biochar;
   [Output] double[] n_demand_bc;
   [Output] double[] n_avail_bc;
   [Output] double[] bc_nh4;
   [Output] double[] BiocharC_labile;
   [Output] double[] BiocharC_resistant;
   [Output] double BiocharC_labile_total;
   [Output] double BiocharC_resistant_total;
   [Output] double rd_hum_fac = 0.0, rd_fr_fom_fac = 0.0, rd_ef_fom_fac = 0.0; 
   [Output] double rd_biom_fac = 0.0, rd_carb_fac = 0.0, rd_cell_fac = 0.0, rd_lign_fac = 0.0, rd_ef_fac = 0.0, rd_fr_fac = 0.0;
   [Output] double[] saxon_bd;
   [Output] double[] saxon_sat;
   [Output] double[] till_bd;
   [Output] double[] till_sat;
   [Output] double[] cnr_bcf; //the cnrf_bc. called as such to avoid a name conflict with SurfaceOrganicMatter (need to look into)
   [Output] double[] scale_factor;
   [Output] double[] dlt_dlayer;
   [Output] double[] biochar_bd; //internal virtual bd for soil water
   [Output] double[] dlt_c_min_biochar_pot;
   [Output] double[] total_stress;
   [Output] double[] soil_cec;
   [Output] double[] soil_cec_orig;
   [Output] double wfps_factor;
      //Debugging related output variables
   //How we communicate with other modules
   [Event] public event BiocharDecomposedDelegate BiocharDecomposed;
   [Event] public event BiocharAppliedDelegate BiocharApplied;
   
   private double[] init_soil_fac;
   private int dayApp;
   private int moApp;
   private int yearApp;

   //kb1 = labile pool, kb2 is resistant (decomp rate constants)
   private double kb1, kb2;
   
   //thisPH = conversion of soil pH, the rest are soil parameters
   private double[] thisPH, dul, ll15, sat, swcon, ks;
   //the date of application
   private DateTime date;
   //nh4 date
   private DateTime nh4_date;
   //The base titration value calcutated from default soil pH
   private double[] titrat_val;
   private double biochar_ph_value; //10^-(bc_ph)
   private double[] yesterday_oc; //OC from yesterday so we can compute delta
   private double rainAmt;
   private double[] initialBD;
   //The ratio of biochar mass in the soil to mass of that segment of soil
   private double[] MassComparison;
   //Respective mass of each layer, computed using bulk density on day 1
   private double[] LayerMass;
   private bool firstTill;
   private int till_depth_layer;
   private double q_ll;//Quality factors
   private string soil_name="Soil";
   ///---------------------------------------------------- Hamze ------------- Varibles replace by GLMS
   private double bc_cce;
   private double bc_cec;
   private double biochar_cn; 
   private double frac_labile; 
   private double frac_c_biochar;
   
   
   // The following event handler will be called once at the beginning of the simulation
   [EventHandler] public void OnInitialised()
   {
      string[] names;
      double[] estimates;
      string[] stringSeparators = new string[] {" "};
      /// Hamze - I'm trying to find the soil name through looping all the childerns of the paddok (Maybe not very efficient but it's working for now)
      pad = MyPaddock.Parent.ChildPaddocks[0];
      foreach (Component s in pad.Children)      {
         if(s.Name.Contains("Water")){
            names = s.Name.Split(stringSeparators, StringSplitOptions.None);    
            soil_name = names[0];
         }
      }
      /////////////////////////////////// Using GLM function to estimate the biochar properties
      estimates=GLMs(feedbio,PTemp);
      frac_c_biochar = estimates[0];
      frac_labile=estimates[1];
      biochar_cn=estimates[2];
      bc_cec=estimates[3];
      bc_cce=estimates[4];
      ///////////////////////////////////
      
      bc_nh4 = new double[oc.Length];
      thisPH = new double[ph.Length];
      soil_cec = new double[oc.Length];
      soil_cec_orig = new double[oc.Length];
      titrat_val = new double[oc.Length];
      MassComparison = new double[oc.Length];
      LayerMass = new double[oc.Length];
      biochar_bd = bd;
      for(int i = 0; i < ph.Length; i++)
      {
         thisPH[i] = ph[i];
         soil_cec[i] = get_soil_CEC(i);
         titrat_val[i] = 216.51*Math.Exp(ph[i]*(-0.91));
         soil_cec_orig[i] = get_soil_CEC(i);
         LayerMass[i] = bd[i] * dlayer[i] * 10000;
      }
      biochar_ph_value = Math.Pow(10, -bc_cce);
      
      //To convert MRT to kb
      
      kb1 = Math.Log(2.0) / (MRT1 * 365);
      kb2 = Math.Log(2.0) / (MRT2 * 365);
      
      init_soil_fac = new double[oc.Length];
      for (int i = 0; i < oc.Length; i++)
      {
         init_soil_fac[i] = 100 / (dlayer[i] * bd[i]);
      }
      
      //Only works for USA format dates - change in future? Maybe change input format?
      dayApp = Convert.ToInt32(date_of_application.Substring(3, 2));
      moApp = Convert.ToInt32(date_of_application.Substring(0, 2));
      yearApp = Convert.ToInt32(date_of_application.Substring(6, 4));
      
      date = new DateTime(yearApp, moApp, dayApp);
      
      
      BiocharC_labile = new double[oc.Length];
      BiocharC_resistant = new double[oc.Length];
      BiocharC = new double[oc.Length];
      
      firstTill = false;
      
      //initialize a lot of things
      dlt_c_biochar_co2 = new double[oc.Length];
      dlt_c_biochar_biom = new double[oc.Length];
      dlt_c_biochar_hum = new double[oc.Length];
      dlt_c_min_biochar = new double[oc.Length];
      dlt_n_min_biochar = new double[oc.Length];
      n_demand_bc = new double[oc.Length];
      n_avail_bc = new double[oc.Length];
      yesterday_oc = new double[oc.Length];
      saxon_bd = new double[oc.Length];
      saxon_sat = new double[oc.Length];
      till_bd = new double[oc.Length];
      till_sat = new double[oc.Length];
      cnr_bcf = new double[oc.Length];
      scale_factor = new double[oc.Length];
      dlt_dlayer = new double[oc.Length];
      dlt_c_min_biochar_pot = new double[oc.Length];
      total_stress = new double[oc.Length];
      
      rainAmt = 0.0;
      initialBD = bd;
      q_ll = 0.01;
    
   }

     
   //Called each daily timestep
   
   
   [EventHandler] void OnProcess()
   {
      //Delta arrays for each variable
      rainAmt += rain;
      
      double[] dlt_ks = new double[oc.Length];
      double[] dlt_dul = new double[oc.Length];
      double[] dlt_ll = new double[oc.Length];
      double[] dlt_bd = new double[oc.Length];
      double[] dlt_swcon = new double[oc.Length];
      double[] dlt_sat = new double[oc.Length];
      double[] dlt_hum = new double[oc.Length];
      double[] dlt_biom = new double[oc.Length];
      double[] dlt_ph = new double[oc.Length];
      double[] dlt_n_avail = new double[oc.Length];
      double[] dlt_biochar_c = new double[oc.Length];
      double[] bc_nh4_dlt = new double[oc.Length];
      double[] dlt_kl = new double[oc.Length];
      
      
      
      for (int i = 0; i < oc.Length; i++)//Since kl's effect is multiplicative, its default needs to be 1
         dlt_kl[i] = 1.0;
      
      //computeDULandBD(out saxon_dul, out saxon_bd, 0);
      //saxon_ll = computeLL(0);
      //saxon_sat = giveSAT(0);
      if (Today < date)
      {
         for (int i = 0; i < dlayer.Length; i++)
         {
            saxon_bd[i] = bd[i];
         }
      }
      if (Today == date)
         //Step 1
         applyBiochar();
      if (Today > date)
      {
         for (int i = 0; i < oc.Length; i++)//try looping through all layers
         {
            MyPaddock.Get(soil_name + " Nitrogen.tf", out tf);//This si why soil name needs to be an input parameter
         
            MassComparison[i] = (BiocharC[i] / frac_c_biochar)/(LayerMass[i]);
            double n_demand, dlt_n_min_tot_bc;
         
         //double rd_hum_fac = 0.0, rd_biom_fac = 0.0, rd_carb_fac = 0.0, rd_cell_fac = 0.0, rd_lign_fac = 0.0, rd_ef_fac = 0.0, rd_fr_fac = 0.0;
            double nh4_change;
         
            double new_ph;
            //Local variables for this specific soil layer
            double new_layer_ll, new_layer_bd, new_layer_dul, new_layer_ks, new_layer_sat, new_layer_swcon = 0.0;
            //step 2
            //When biochar functionality is expanded, every instance of a '0' method argument or array index will be changed to a layer index, and layers that biochar 
            //alters will be iterated over in a for loop, but for now biochar only changes the first layer
            dlt_c_min_biochar[i] = computeDailyBCCarbDecomp(i);
            //step 3
            computeDLTs(out dlt_c_biochar_biom[i], out dlt_c_biochar_hum[i], out dlt_c_biochar_co2[i], dlt_c_min_biochar[i]);
            //step 4 -inactive
            
         
            n_demand = getNDemand(dlt_c_biochar_biom[i], dlt_c_biochar_hum[i]);
            dlt_n_min_tot_bc = computeNFromDecomp(dlt_c_min_biochar[i]);
         
            n_demand_bc[i] = n_demand;
            n_avail_bc[i] = dlt_n_min_tot_bc;
            dlt_n_min_biochar[i] = dlt_n_min_tot_bc - n_demand; //This will get added to dlt_n_min_tot I think
            //Step 5 happens in model
         
            //Step 6 
         
            get_rd_factors(out rd_hum_fac, out rd_biom_fac, out rd_carb_fac, out rd_cell_fac, out rd_lign_fac, 
               out rd_ef_fac, out rd_fr_fac, out rd_ef_fom_fac, out rd_fr_fom_fac, i);
            
            //Step 7 
         
            nh4_change = get_NH4_changes(i);
            //Step 8
         
            soil_cec[i] = get_new_cec(i);
            new_ph = get_new_ph(i);
         
            //For computing delta locally.
         
            getCurrentSoilWatValues();
         
         
            //Step 9
            new_layer_ll = computeLL(i);
            computeDULandBD(out new_layer_dul, out new_layer_bd, i);
            
            //new_layer_sat = giveSAT(i); //Active but not being used
            /**
            new_layer_swcon = computeSWCON(0, new_layer_sat, new_layer_bd); 
            new_layer_ks = computeKS(0, new_layer_sat, new_layer_dul, new_layer_ll);
            **/
         
         
            //End of steps
            /**
            * The biochar decomposed event requires that changes be in terms of delta. 
            * However, our equations give the total value, not the change, so we must compute
            * the change within this script.
            **/
            //dlt_ks[0] = new_layer_ks - ks[0];
            dlt_dul[i] = new_layer_dul;// -dul[i];
            dlt_ll[i] = new_layer_ll;// -ll15[i];
        
         
         
         //not actually a delta, model stops working if it is. Instead, is the next wanted value of bd
            if (BiocharC[i] != 0.0)
            {
               dlt_bd[i] = new_layer_bd + saxon_bd[i];
            }
            else
               dlt_bd[i] = initialBD[i];
            
         
            dlt_hum[i] = dlt_c_biochar_hum[i];
            dlt_biom[i] = dlt_c_biochar_biom[i];
         
            dlt_biochar_c[i] = dlt_c_min_biochar[i];
         
            dlt_ph[i] = new_ph - ph[i];
         
            dlt_n_avail[i] = dlt_n_min_biochar[i];
         
            bc_nh4_dlt[i] = nh4_change;
            if (kl_switch.Equals("on")) //So that kl does not go to 0
               dlt_kl[i] = 1; //what became of step 10
            
            
         }
            //End of loop
            //The data structure for our decomposition event
            BiocharDecomposedType BiocharDecomp = new BiocharDecomposedType();
         
         getCurrentSoilWatValues();
         //region for andales saxon mergeing - to later integrate with 
         double[] andales_bd = AndalesBD();
         double[] bd_new = biggest_bd_dlt(dlt_bd, andales_bd);
         double[] sat_dlt_new = sat_in_terms_of_dlt(bd_new);
         
         for (int i = 0; i < oc.Length; i++)
         {
            double temp;
            saxon_sat[i] = (-(dlt_bd[i] - saxon_bd[i]) / 2.65) * 0.9 + sat[i];
            saxon_bd[i] = dlt_bd[i];
            till_bd[i] = andales_bd[i];
            till_sat[i] = ( -(andales_bd[i] - biochar_bd[i]) / 2.65) * 0.9 + sat[i];
            temp = 100 / (bd_new[i] * init_soil_fac[i]);
            dlt_dlayer[i] = temp - dlayer[i];
         }
         
         biochar_bd = bd_new;
            //Script control area
            if (dul_switch.Equals("on"))
            {
               BiocharDecomp.dlt_dul = dlt_dul;
            }
            if (ll_switch.Equals("on"))
            {
               BiocharDecomp.dlt_ll = dlt_ll;
            }
            if (sat_switch.Equals("on"))
            {
               BiocharDecomp.dlt_sat = sat_dlt_new;
            }
         //Since errors occur if bd is in terms of delta, we need to ensure that if bd is off, we still get what we want
            if(bd_switch.Equals("on"))
            {
            BiocharDecomp.dlt_bd = bd;   
            //BiocharDecomp.dlt_bd = bd_new;
               //MyPaddock.Set("dlt_dlayer", dlt_dlayer);
            }
            else 
            {
               BiocharDecomp.dlt_bd = bd;
            }
            if (swcon_switch.Equals("on"))
            {
               BiocharDecomp.dlt_swcon = dlt_swcon;
            }
            if (ks_switch.Equals("on"))
            {
               BiocharDecomp.dlt_ks = dlt_ks;
            }
         
         
            if (biochar_c_switch.Equals("on"))
            {
               BiocharDecomp.hum_c = dlt_hum;
               BiocharDecomp.biom_c = dlt_biom;
               BiocharDecomp.dlt_biochar_c = dlt_biochar_c;
            }
         
            if (nitrification_switch.Equals("on"))
            {
               BiocharDecomp.bc_nh4_change = bc_nh4_dlt;
            }
         
            if (decomp_switch.Equals("on"))
            {
               BiocharDecomp.dlt_rd_hum = rd_hum_fac;
               BiocharDecomp.dlt_rd_biom = rd_biom_fac;
               BiocharDecomp.dlt_rd_carb = rd_carb_fac;
               BiocharDecomp.dlt_rd_cell = rd_cell_fac;
               BiocharDecomp.dlt_rd_lign = rd_lign_fac;
               BiocharDecomp.dlt_rd_ef = rd_ef_fac;
               BiocharDecomp.dlt_rd_fr = rd_fr_fac;
            BiocharDecomp.dlt_rd_ef_fom = rd_ef_fom_fac;
            BiocharDecomp.dlt_rd_fr_fom = rd_fr_fom_fac;
            
            }
         
         
            BiocharDecomp.dlt_n_biochar = dlt_n_avail;
            if (ph_switch.Equals("on"))
            {
               BiocharDecomp.dlt_ph = dlt_ph;
            }
         
         BiocharDecomp.bc_wfps_factor = 1.0 - this.bc_wfps_factor;
         
         
            BiocharDecomp.dlt_kl = dlt_kl; //Since KL is a multiplicative effect, if we do not always assign this KL will go to 0
            //If uninitialized, it is 0 by default
         
            BiocharDecomposed.Invoke(BiocharDecomp);
         Console.WriteLine("Biochar bd: " + biochar_bd[0]);
         
         
      }
      
      
      for (int i = 0; i < oc.Length; i++)
         yesterday_oc[i] = oc[i]; //Make a deep copy
      
      
   }
      
   
   //Step 1 section
   private void applyBiochar()
   {
      /**
      * This puts the proper amount of biochar into the necessary soil layers
      * based off of how deeply the biochar was applied, assuming even distribution
      * of BC throughout its application range. Based off an implementation already 
      * in APSIM.
      **/
      wfps_factor = 1.0 - bc_wfps_factor;
      double depth_so_far = 0.0;
      double depth_to_go;
      double frac_bc_layer;
      double layer_incorp_depth;
      for (int i = 0; i < dlayer.Length; i++)
      {
         depth_to_go = incorp_depth - depth_so_far;
         if (depth_to_go <= 0.0)
            depth_to_go = 0.0;
         layer_incorp_depth = Math.Min(depth_to_go, dlayer[i]);
         frac_bc_layer = layer_incorp_depth / incorp_depth;
         BiocharC[i] = biochar_added * frac_c_biochar * (1 - biochar_loss) * frac_bc_layer;
         BiocharC_labile[i] = BiocharC[i] * frac_labile;
         BiocharC_resistant[i] = BiocharC[i] * (1 - frac_labile);
         
         depth_so_far += dlayer[i];
      }
      BiocharAppliedType BioApp = new BiocharAppliedType();
      double[] bc_carb_applied = new double[oc.Length];
      for (int i = 0; i < bc_carb_applied.Length; i++)
         bc_carb_applied[i] = BiocharC[i];
      
      BioApp.bc_carbon_ammount = bc_carb_applied;
      BiocharApplied.Invoke(BioApp);
         
      Console.WriteLine("Biochar has been applied \nAmmount: " + biochar_added + " kg/ha" + "\nDepth: " + incorp_depth + " (mm)");
      sumSoilBCFirstTime();
      
   
   }
   //Step 2 section
   
   private double computeDailyBCCarbDecomp(int layer)
   {
      double pot_hum, pot_biom, pot_co2, pot_tot;
      cnr_bcf[layer] = calculateCNR_BCF(layer);
      calculatePotentialDecomp(layer, cnr_bcf[layer], out pot_hum, out pot_biom, out pot_co2, out pot_tot);
      scale_factor[layer] = calculateScale(layer, pot_biom, pot_hum, pot_tot);
      
      
      return calculateActualDecomp(layer, scale_factor[layer], cnr_bcf[layer]);
   }
   
   //Helper methods for biochar decomposition 
   private double calculateCNR_BCF (int layer)
   {
      double cnr_bc; //Biochar cn ratio for decomposition
      double n_available_cnr; // Potential nitrogen available for bc decomposition?
      
      //bc available N + mineral N in layer
      n_available_cnr = (BiocharC_labile[layer] / biochar_cn) + nh4[layer] - nh4_min[layer] + no3[layer] - no3_min[layer];
      if (n_available_cnr != 0.0)
         cnr_bc = (BiocharC_labile[layer] / n_available_cnr);
      else
         cnr_bc = 0.0;
      
      
      double cnrf_bc = Math.Exp(-cnrf_bc_coeff * (cnr_bc - cnrf_bc_optcn) / cnrf_bc_optcn);
      if (cnrf_bc > 1)
         cnrf_bc = 1;
      if (cnrf_bc < 0)
         cnrf_bc = 0;
      
      return cnrf_bc;
   }
   
   private void calculatePotentialDecomp(int layer, double cnrf_bc, out double pot_hum, out double pot_biom, out double pot_co2, out double pot_tot)
   {
      double pot_labile = BiocharC_labile[layer] * kb1 * Math.Min( Math.Min(wf , tf[layer]) , cnrf_bc);
      double pot_resist = BiocharC_resistant[layer] * kb2 * Math.Min( Math.Min(wf , tf[layer]) , cnrf_bc);
      
      pot_tot = pot_labile + pot_resist;
      dlt_c_min_biochar_pot[layer] = BiocharC_labile[layer] * kb1 + BiocharC_resistant[layer] * kb2;
      total_stress[layer] = Math.Min( Math.Min(wf , tf[layer]) , cnrf_bc);
      pot_co2 = pot_tot * (1 - ef_biochar);
      pot_biom = pot_tot * ef_biochar * fr_biochar_biom;
      pot_hum = pot_tot * ef_biochar * (1 - fr_biochar_biom);
   }
   
   private double calculateScale(int layer, double pot_biom, double pot_hum, double pot_tot)
   {
      double bc_n_min_tot = pot_tot / biochar_cn;
      double n_demand = (pot_biom / biom_cn) + (pot_hum / soil_cn);
      //Calculate n available from mineral n
      double n_avail = nh4[layer] - nh4_min[layer] + no3[layer] - no3_min[layer] + bc_n_min_tot;
      
      double scale_of;
      
      if (n_demand > n_avail)
      {
         scale_of = (nit_tot[layer] / (n_demand - bc_n_min_tot));
         if (scale_of > 1)
            scale_of = 1;
         
      }
      else
         scale_of = 1;
      
      return scale_of;
      
   }
   
   //Performs the actual decomposition of biochar, based off of the limitations the potential runs into
   private double calculateActualDecomp(int layer, double scale_of, double cnrf_bc)
   {
      double decomp1 = BiocharC_labile[layer] * kb1 * Math.Min( Math.Min(wf , tf[layer]) , cnrf_bc) * scale_of;
      BiocharC_labile[layer] -= decomp1;
      double decomp2 = BiocharC_resistant[layer] * kb2 * Math.Min( Math.Min(wf , tf[layer]) , cnrf_bc) * scale_of;
      BiocharC_resistant[layer] -= decomp2;
      if (BiocharC_labile[layer] < 0)
         BiocharC_labile[layer] = 0;
      if (BiocharC_resistant[layer] < 0)
         BiocharC_resistant[layer] = 0;
      
      
      BiocharC[layer] = BiocharC_labile[layer] + BiocharC_resistant[layer]; //Remove decomposed ammount from biochar pool
      
      updateSoilBCTotals(layer);
      return decomp1 + decomp2;
   }
   
   //End of decomposition helper methods
   
   //Computes the changes in co2, biom c and humic c due to a change in biochar c
   
   //Step 3 section
   private void computeDLTs(out double dlt_c_biochar_biom, out double dlt_c_biochar_hum, out double dlt_c_biochar_co2, double dlt_c_min_biochar)
   {
      dlt_c_biochar_co2 = dlt_c_min_biochar * (1 - ef_biochar);
      dlt_c_biochar_biom = dlt_c_min_biochar * ef_biochar * fr_biochar_biom;
      dlt_c_biochar_hum = dlt_c_min_biochar * ef_biochar * (1 - fr_biochar_biom);
      
      
   }
   
   //Step 4 section 
   private double getNDemand(double dlt_c_biochar_biom, double dlt_c_biochar_hum)
   {
      return (dlt_c_biochar_biom / biom_cn) + (dlt_c_biochar_hum / soil_cn);  // this biochar n demand, n_demand_bc
   }
   
   private double computeNFromDecomp(double dlt_c_min_biochar) 
   {
      return (dlt_c_min_biochar / biochar_cn); // this is n released during biochar decomposition, n_avail_bc
   }
   
   
   
   //Step 5 happens within the apsim model itself 
   
   //Step 6 
   private void get_rd_factors(out double rd_hum_fac, out double rd_biom_fac, out double rd_carb_fac,
      out double rd_cell_fac, out double rd_lign_fac, out double rd_ef_fac, out double rd_fr_fac, out double rd_ef_fom_fac,
      out double rd_fr_fom_fac, int layer)
   {
      
         rd_hum_fac = (prim_hum * BiocharC_total / 10000);
         rd_biom_fac = (prim_biom * BiocharC_total / 10000);
         rd_carb_fac = (prim_carb * BiocharC_total / 10000);
         rd_cell_fac = (prim_cell * BiocharC_total / 10000);
         rd_lign_fac = (prim_lign * BiocharC_total / 10000);
         rd_ef_fac = (prim_ef * BiocharC_total / 10000);
         rd_fr_fac = (prim_fr * BiocharC_total / 10000);
         rd_ef_fom_fac = (prim_ef_fom * BiocharC_total / 10000);
         rd_fr_fom_fac = (prim_fr_fom * BiocharC_total / 10000);
         
         
         
      
   }
   
   //Step 7 big work here pretty sure does not matter ppm or kg/ha since both related by constant
   private double get_NH4_changes(int layer)
   {
      if (BiocharC[layer] > 0.0)
      {
         double cec_ratio = soil_cec[layer] / soil_cec_orig[layer];
         double nh4_absorbed = nh4[layer] * cec_ratio * nh4_adsorption / (1 + cec_ratio * nh4_adsorption);
         double nh4_desorbed = bc_nh4[layer] * cec_ratio * nh4_desorption / (1 + cec_ratio * nh4_desorption);
         double nh4_change = nh4_desorbed - nh4_absorbed;
         bc_nh4[layer] = bc_nh4[layer] - nh4_change;
         return nh4_change;
      }
      else 
         return 0.0;
   }
   
   
   //Step 8
   private double get_new_ph(int layer)
   {
      return compute_ph_equation(soil_cec[layer], ab_val, layer) + compute_bc_limeing(layer);
     
      
      
   }
   
   
   //Step 9 area - Changes to DUL and LL and the like 

   
   private double computeLL(int layer)
   {
      
     
      double dlt_oc = oc[layer] - yesterday_oc[layer];
      
      //double ll15_temp = -0.024 * sand + 0.487 * clay + 0.006 * om + 0.005 * sand * om - 0.013 * clay * om + 0.068 * sand * clay + 0.031;
      //ll15_temp = ll15_temp + 0.14 * ll15_temp - 0.02;
      if (BiocharC[layer] != 0.0)
      {
         return q_ll * (0.0118 + 0.0098 * sand - 0.0255 * clay) * dlt_oc;
      }
      else 
         return 0.0;
      
   }
   
   //Returns a two element array containing values used in computing DUL and bd for a specific layer, as well as in sat
   private double[] computeDULMidway(int layer)
   {
      
      double om;
      //four temporary values needed 
      double temp1, temp2, temp3, temp4;
      
      //Based off of documentation equations
      
     /**
      temp1 = -0.251 * sand + 0.195 * clay + 0.011 * om + 0.006 * sand * om - 0.027 * clay * om + 0.452 * sand * clay + 0.299;//dula
      temp1 = temp1 + (1.283 * temp1 * temp1 - 0.374 * temp1 - 0.015);//dulb
      temp2 = -0.097 * sand + 0.043;//dulc
      temp3 = 0.278 * sand + 0.034 * clay + 0.022 * om - 0.018 * sand * om - 0.027 * clay * om - 0.584 * sand * clay + 0.078;//duld
      temp3 = temp3 + (0.636 * temp3 - 0.107);//dule
      temp4 = temp1 + temp3;//dulf
      temp4 = temp4 + temp2;//dulg
      **/
      
      //Returns return[0] = DULh from documentation, return[1] = DULb from documentation, as both values are needed elsewhere
      return new double[] {0.0, 0.0};
      
      
      
   }
   
   //Computes DUL and BD based off of changes to soil OC due to biochar in that layer
   private void computeDULandBD(out double layer_dul, out double layer_bd, int layer)
   {
      
      
      double dlt_oc = oc[layer] - yesterday_oc[layer];
      
      double q_dul = 1.3067 * Math.Exp(-dul_qual * oc[layer]);
      double q_bd = 1.3067 * Math.Exp(-bd_qual * oc[layer]);
      /**
      midDUL = computeDULMidway(layer);
      BDa = midDUL[0] * df;
      
      gravels = ((BDa / 2.65) * gravelw) / (1 - gravelw * (1 - BDa / 2.65));
      **/
      if (BiocharC[layer] != 0.0)
      {
         layer_bd = q_bd * (-0.2332 + 0.115 * sand + 0.35 * clay) * dlt_oc;
      
         layer_dul = q_dul * (0.0261 + 0.0072 * sand - 0.0561 * clay) * dlt_oc;
      }
      else
      {
         layer_bd = 0.0;
         layer_dul = 0.0;
      }
      
      
      
   }
      
   //Gives the SAT for a given layer based off of the layer's newly computed bulk density
   /**
   private double giveSAT (int layer)
   {
      
      
      
      double dlt_oc = oc[layer] - yesterday_oc[layer];
      if (BiocharC[layer] != 0.0)
      {
         return ( 0.0836 - 0.0412 * sand - 0.1255 * clay)  * dlt_oc;
      }
      else
         return 0.0;
   }
   **/
   
   //deprecated
   private double computeKS(int layer, double layer_sat, double layer_dul, double layer_ll)
   {
      return 0.0;
      
      
   }
   //deprecated
   private double computeSWCON(int layer, double layer_sat, double layer_dul)
   {
      double SWCON = (layer_sat / 0.95 - layer_dul) / (layer_sat / 0.95);
      
      return SWCON;
      
   }
   
   //Gets the current values of various soil water associated variables so we can compute our dlts based off of the difference
   private void getCurrentSoilWatValues()
   {
      
      MyPaddock.Get(soil_name + " Water.dul", out dul);
      MyPaddock.Get(soil_name + " Water.ll15", out ll15);
      MyPaddock.Get(soil_name + " Water.sat", out sat);
      //MyPaddock.Get(soil_name + " Water.bd", out bd);
      MyPaddock.Get(soil_name + " Water.swcon", out swcon); //doesn't work
      MyPaddock.Get(soil_name + " Water.ks", out ks);
   }
   

   //Updates the total amount of soil biochar in the system. We need to make how this is done better, so it is only run once,
   //as it is a O(n) operation being run within a O(n) operation, making our whole daily algorithm O(n^2) when it doesn't 
   //need to be. Done?
   private void updateSoilBCTotals(int layer)
   {
      if (layer == 0)//Zero all values the first time this is called each day
      {
         BiocharC_total = 0;
         BiocharC_labile_total = 0;
         BiocharC_resistant_total = 0;
      }
      BiocharC_total += BiocharC[layer];
      BiocharC_labile_total += BiocharC_labile[layer];
      BiocharC_resistant_total += BiocharC_resistant[layer];
   }
   
   
   //Special method that sums the total BC in the system. Used only during the apply biochar process.
   private void sumSoilBCFirstTime()
   {
      BiocharC_total = 0;
      BiocharC_labile_total = 0;
      BiocharC_resistant_total = 0;
      for (int i = 0; i < dlayer.Length; i++)
      {
         BiocharC_total += BiocharC[i];
         BiocharC_labile_total += BiocharC_labile[i];
         BiocharC_resistant_total += BiocharC_resistant[i];
      }
      
   }
   //For the andales methods of bd
   [EventHandler] void OnTillage(TillageType Till)
   {
      firstTill = true;
      
      rainAmt = 0.0;
      float depth = Till.tillage_depth;
      for (int i = 0; i < oc.Length; i++)
      {
         depth -= (float) dlayer[i];
         till_depth_layer = i;
         if ( depth <= 0)
         {
            break;  
         }
         
      }
   }
   //Method that gives BD computed with the andales equation
   private double[] AndalesBD()
   {
      double[] ret = new double[oc.Length];
      double q_bd; 
      double q_const = 1.3067;
      
      for (int layer = 0; layer < ret.Length; layer++)
      {
         if (oc[layer] < 0.5)
            q_const = 1.8067;
         if (firstTill && layer <= till_depth_layer)
         {
            q_bd = q_const * Math.Exp(-bd_qual * oc[layer]);
            ret[layer] = q_bd * (tillage * initialBD[layer] - initialBD[layer]) * Math.Exp(-(5 * (1 - 0.205 * oc[layer])) * rainAmt * 0.00217);//Based off of Andales equation
            ret[layer] = ret[layer] + initialBD[layer];
            if (ret[layer] < initialBD[layer] - ((1.0 - tillage) * initialBD[layer] * q_bd))//really confusing. basically if it is lower than it could possibly be (the andales equation fails to capture reality for oc > 4.8) 
               ret[layer] = initialBD[layer] - ((1.0 - tillage) * initialBD[layer] * q_bd);// we set it to the lowest possible and regard it as (mostly) constant
         }
         else
            ret[layer] = initialBD[layer];
      }
      
      return ret;
   }
   //Computes the biggest delta associated with bd
   private double[] biggest_bd_dlt(double[] dlt_bd, double[] andales_bd)
   {
      double[] ret = new double[saxon_bd.Length];
      for (int layer = 0; layer < oc.Length; layer++)
      {
         if (Math.Abs(dlt_bd[layer] - initialBD[layer])>= Math.Abs(andales_bd[layer] - initialBD[layer]))
         {
            ret[layer] = dlt_bd[layer];
         }
         else
            ret[layer] = andales_bd[layer];
      }
      
      return ret;
   }
   
   private double[] sat_in_terms_of_dlt(double[] bd_new)
   {
      double[] ret = new double[bd_new.Length];
      if (bd_switch == "on")
      {
         for (int layer = 0; layer < bd_new.Length; layer++)
         {
         
         
            ret[layer] = (-(bd_new[layer] - biochar_bd[layer]) / 2.65) * 0.9;
         
         }
      }
      else//still need SAT estimation if no bd change... (old... maybe unneccessary with new change?? but then bd off is unmeaningful)
      {
         for (int layer = 0; layer < oc.Length; layer++)
         {
            double dlt_oc = oc[layer] - yesterday_oc[layer];
            if (BiocharC[layer] != 0.0)
            {
               ret[layer] = ( 0.0836 - 0.0412 * sand - 0.1255 * clay) * dlt_oc;
            }
            else
               ret[layer] = 0.0;
            
         }
      }
      return ret;
   }
   
   
   //Computes soil CEC on the first day of the simulation to have a constant value for soil CEC
   //Which is later used as a base for when BC is applied (i am not sure if we should maintain pH at all?)
   private double get_soil_CEC(int layer)
   {
      if (soil_order == "Aridisol")
      {
         return Math.Exp(0.042 * Math.Log(oc[layer]) + 0.828 * Math.Log(nclay_portion * clay * 100) + 0.236);
      }
      else if (soil_order == "Entisol")
      {
         return Math.Exp(0.078 * Math.Log(oc[layer]) + 0.873 * Math.Log(nclay_portion * clay * 100) + 0.084);
      }
      else if (soil_order == "Gelisol")
      {
         return Math.Exp(0.359 * Math.Log(oc[layer]) + 0.49 * Math.Log(nclay_portion * clay * 100) + 1.05);
      }
      else if (soil_order == "Inceptisol")
      {
         return Math.Exp(0.134 * Math.Log(oc[layer]) + 0.794 * Math.Log(nclay_portion * clay * 100) + 0.239);
      }
      else if (soil_order == "Mollisol")
      {
         if (oc[layer] < 0.3)
            return Math.Exp(0.932 * Math.Log(nclay_portion * clay * 100) - 0.174);
         else
            return Math.Exp(0.113 * Math.Log(oc[layer]) + 0.786 * Math.Log(nclay_portion * clay * 100) + 0.475);
      }
      else if (soil_order == "Vertisol")
      {
         return Math.Exp(0.059 * Math.Log(oc[layer]) + 0.86 * Math.Log(nclay_portion * clay * 100) + 0.312);
      }
      else if (soil_order == "Histosol")
      {
         return Math.Exp(0.319 * Math.Log(oc[layer]) + 0.497 * Math.Log(nclay_portion * clay * 100) + 1.075);
      }
      else if (soil_order == "Alfisol")
      {
         if (oc[layer] < 0.3)
            return Math.Exp(0.911 * Math.Log(nclay_portion * clay * 100) - 0.308);
         else
            return Math.Exp(0.158 * Math.Log(oc[layer]) + 0.805 * Math.Log(nclay_portion * clay * 100) + 0.216);
      }
      else if (soil_order == "Spodosol")
      {
         return Math.Exp(0.045 * Math.Log(oc[layer]) + 0.798 * Math.Log(nclay_portion * clay * 100) + 0.029);
      }
      else if (soil_order == "Ultisol")
      {
         return Math.Exp(0.184 * Math.Log(oc[layer]) + 0.57 * Math.Log(nclay_portion * clay * 100) + 0.365*Math.Log((1 - clay - sand)*100) - 0.906);       
      }
      else if (soil_order == "Oxisol")
      {
         return 2.738 * oc[layer] + 0.103 * nclay_portion * clay * 100 + 0.123 * (100*(1 - clay - sand)) - 2.531;
      }
      else 
         return 20.0;
      
   }
   //Computes a new biochar adjusted value for soil CEC
   private double get_new_cec(int layer)
   {
      return soil_cec_orig[layer] * (1 - MassComparison[layer]) + bc_cec * MassComparison[layer];
   }
   //Computes a value for soil pH based off of the given values for CEC and acid, using that layer's particular titration curve value
   private double compute_ph_equation(double cec_val, double acid, int layer)
   {
      return (uph - lph) / (1 + titrat_val[layer] * Math.Exp(-acid/cec_val)) + lph;
   }
   //Computes a value that describes the lime effect that biochar has on soils
   private double compute_bc_limeing(int layer)
   {
      double bc_alkaline = MassComparison[layer] * bc_cce;
      return bc_alkaline * (uph - thisPH[layer]) * (thisPH[layer] - lph) * bot_slope / ((uph - lph) * soil_cec[layer]); 
   }
   
   
   // based on the selected feedstock class and biomass I choose the GLM and compute the params
   /// <summary>
   /// Estimates the biochar properties given Biomass, Feedstock and Temp
   /// </summary>
   /// <param name="B">Biomass</param>
   /// <param name="T">Temperature</param>
   /// <returns></returns>
   private double[] GLMs(string B, double T){
      // Coefficients are taken from here:https://veromora.shinyapps.io/BiocharEng/
      //http://www.sciencedirect.com/science/article/pii/S0960852415002138
     // first elem is frac_c_biochar, frac_labile, biochar_cn, biochar_cce, bc_cec
      double[] outputs = new double[5];
      double c1=0;
      double c2 = 0;
      double c3 = 0;
      double c4 = 0;
      double c5 = 0;
      double c6 = 0;
      double c7 = 0;
      double c8 = 0;
      double c9 = 0;
      switch (B)
      {
         case "Corn":
            c1 = 18.2857;
            c2 = 0.3642;
            c3 = 0.0812;
            c4 = -4.043;
            c5 = -0.006;
            c7 = 0.008923;
            c8 = 0.2268;
            c9 = -0.0003498;
            break;
         case "Manure":
            c1 = 46.60714;
            c2 = 0.15786;
            c3 = 0.085492;
            c4 = 11.31;
            c5 = -0.01412;
            c6 = 0.2675;
            c7 = 0.008923;
            c8 = 0.1769;
            c9 = -0.0004661;
            break;
         case "Oak":
            c1 = -48.44101;
            c2 = 0.8028;
            c3 = 0.1166;
            c4 = -15.23;
            c5 = 0.001536;
            c6 = -3.2516;
            c7 = 0.008923;
            c8 = 0.2446;
            c9 = -0.0007776;
            break;
         case "FoodWaste":
            c1 = 599.07143;
            c2 = -1.42;
            c3 = 0.01705;
            c4 = 85.47;
            c5 = -0.1564;
            c6 = -0.095;
            c7 = 0.008923;
            c8 = 0.0205;
            c9 = -0.0007561;
            break;
         case "Grass":
            c1 = -131.02857;
            c2 = 0.928;
            c3 = 0.1038;
            c4 = -5.7843;
            c5 = -0.0024;
            c6 = -1.433;
            c7 = 0.008923;
            c8 = 0;
            c9 = 0;
            break;
         case "Pine":
            c1 = -127.755;
            c2 = 0.9179;
            c3 = 0.1147;
            c4 = -16.873;
            c5 = 0.001184;
            c6 = -3.065;
            c7 = 0.008923;
            c8 = 0.0916;
            c9 = -0.0003826;
            break;
         case "Polutry":
            c1 = -139.3619;
            c2 = -0.0886;
            c3 = -0.01538;
            c4 = 13.543;
            c5 = -0.03757;
            c6 = 0.745833;
            c7 = 0.008923;
            c8 = 0.3711;
            c9 = -0.001287;
            break;
         case "Hazelnut":
            c1 = 72.8214;
            c2 = 0.49929;
            c3 = 0.09939;
            c4 = -12.413;
            c5 = -0.0004286;
            c6 = -1.2275;
            c7 = 0.008923;
            c8 = -0.3006;
            c9 = -0.000002046;
            break;
      }
      outputs[0] = (483.9285) + (c1) + (c2) * T;
      outputs[1] = (100-(9.43219+(c3*T)))/100;//because it stimates the fixed carobn
      outputs[2] =  outputs[1]/(10.77)+(c4)+(c5*T); // C/N
      outputs[3] = 5.077 + (c6) + (c7 * T);
      outputs[4] = 1.7 + (c8) + (c9) * T;
      return(outputs);
   }

//TODO sand clay gravelw into arrays and gotten from simulation
  

}