package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class BulkRebateUpdateRequest implements Serializable {

  private static final long serialVersionUID = -3314318180748157565L;
  private int month;
  private int year;
  private String businessPartnerCode;
  private String c1CategoryCode;
  private String c2CategoryCode;
  private String brandName;
  private double rebate;
}
