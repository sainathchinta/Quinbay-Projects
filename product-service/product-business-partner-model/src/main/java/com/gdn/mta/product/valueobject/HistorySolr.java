package com.gdn.mta.product.valueobject;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class HistorySolr {
  private String id;
  private Date accessTime;
  private String productSku;
  private String gdnSku;
  private String gdnName;
  private String activity;
}
