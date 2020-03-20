import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def get_term(years,ppy=12):
    
    # term = number of payments
    
    return(years*ppy)

def get_periodic_int_rate(air,ppy=12):
    
    # periodic interest rate = (annual interest rate) x (payments per year)
    
    return((air/100) / ppy)

def emi_calc(P,air,years,ppy=12):
    
    # Retruns Monthly Payment (Equated Monthly Installment, EMI) with fees
    
    # P = principal (amount borrowed)
    # air = annual interest rate (as a %)
    # years = number of years of loan
    # ppy = payments per year (12 if paying monthly)
    
    # n = term (number of payments)
    # r = periodic interest rate
    
    n = get_term(years, ppy)
    r = get_periodic_int_rate(air,ppy)
    
    numerator = (r *((1 + r)**(n)) )
    denominator = ((1 + r)**(n)) - 1
    emi = P * (numerator / denominator)
    
    return(np.round(emi,2))

def emi_with_fees(Po,air,years,fees=[0], ppy=12):
    
    # Retruns Monthly Payment (Equated Monthly Installment, EMI) with fees
    
    # P = principal (amount borrowed)
    # air = annual interest rate (as a %)
    # years = number of years of loan
    # ppy = payments per year (12 if paying monthly)
    
    # n = term (number of payments)
    # periodic interest rate
    
    
    fee_sum = sum(fees)
    n = get_term(years, ppy)
    r = get_periodic_int_rate(air,ppy)
    P = Po + fee_sum
    
    numerator = (r *((1 + r)**(n)) )
    denominator = ((1 + r)**(n)) - 1
    emi = P * (numerator / denominator)

    return(np.round(emi,2))

def tot_interest_paid(P,emi,periodic_int_rate):
    
    interest_paid = np.floor((periodic_int_rate*P)*100)/100
    
    principal_paid = np.round(emi-interest_paid, 2)
    
    new_balance = np.round(P - principal_paid,2)
    
    return(emi, interest_paid, principal_paid, new_balance)

def make_payment_table(Po, air, years, fees=[0], ppy=12, add_emi=0):
    
    payment_list = []
    
    term = get_term(years, ppy)
    
    emi = emi_with_fees(Po,air,years,fees,ppy)
    
    emi = emi + add_emi
    
    periodic_int_rate = get_periodic_int_rate(air,ppy)
    
    P = Po + sum(fees)
    
    for n in range(1, term + 1):
        emi,i_paid,p_paid,new_p = tot_interest_paid(P, emi, periodic_int_rate)
        
        payment_list.append([n, P, emi, i_paid, p_paid, new_p])
        
        #Update principal (starting balance for next calculation)
        P = np.round(new_p,2)
        
        if new_p <= 0:
            break
    
    
    c_names = ['Month','Starting Balance','Repayment','Interest Paid','Principal Paid','New Balance']
    payment_table = pd.DataFrame(payment_list, columns = c_names)
    
    
    return(payment_table)

def nb_print_int_princpl(payment_table, ylimit=700):
    
    # payment_table = dataframe, generate with make_payment_table
    
    fig, axes = plt.subplots(nrows = 1, ncols = 1, figsize=(10, 5) )
    axes.plot(payment_table['Month'], payment_table['Principal Paid'], c = 'b', label = 'Principal');
    axes.plot(payment_table['Month'], payment_table['Interest Paid'], c = 'k', label = 'Interest');

    axes.set_xlim((1, 60));
    axes.set_xticks([1, 10, 20, 30, 40, 50, 60])
    axes.set_ylim((0, ylimit));
    axes.set_ylabel('Dollars', fontsize = 22);
    axes.set_xlabel('Month', fontsize = 22);

    plt.xticks(fontsize = 20)
    plt.yticks(fontsize = 20)
    axes.set_title('Interest and Principal Paid Each Month', fontsize = 24)

    plt.legend(bbox_to_anchor=(1.02,0), loc="lower left", borderaxespad=0, fontsize = 20)
    plt.tight_layout()
    plt.grid(axis = 'both')
    plt.savefig('Interest_Principal.png', dpi = 1000)
    
    return()

    


if __name__ == '__main__':
    print("Calculate Equated Monthly Installment: ")
    print(emi_calc(31115 * (1.075),  7.02, 5))
    
    print("Calculate Equated Monthly Installment with fees: ")
    sales_tax = (32615 * 0.0975)
    print(emi_with_fees(31115,  7.02, 5, [sales_tax, 50, 200, 65, 80]))
    
    print("Calculate Total Interest Paid:")
    payment_table = make_payment_table(34689.96,7.02,5)
    print(payment_table.head())
    print(payment_table.tail())
    print("\nIterest Paid:")
    print(np.round(payment_table['Interest Paid'].sum(), 2))
    
    

    
    
