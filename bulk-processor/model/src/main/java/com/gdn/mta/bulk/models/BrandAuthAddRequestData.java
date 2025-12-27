package com.gdn.mta.bulk.models;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BrandAuthAddRequestData {
  private String brandName;
  private String brandCode;
  private String sellerCode;
  private String authorisationStatus;
  private Date authStartDate;
  private Date authExpireDate;
  private List<String> documentLinks;
  private int excelRowNumber;
}
