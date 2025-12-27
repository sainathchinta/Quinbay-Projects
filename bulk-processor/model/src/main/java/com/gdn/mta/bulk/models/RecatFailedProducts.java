package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class RecatFailedProducts {

  String productCode;
  String productName;
  String masterCategoryCode;
  String masterCategoryName;
  String newMasterCategoryCode;
  String newMasterCategoryName;
  String errorMessage;
}