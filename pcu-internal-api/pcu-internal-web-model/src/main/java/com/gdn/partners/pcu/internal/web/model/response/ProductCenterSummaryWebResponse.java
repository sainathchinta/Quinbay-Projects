package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCenterSummaryWebResponse {
  private String productSku;
  private String productName;
  private String status;
  private MasterCategoryWebResponse masterCategory;
  private List<String> salesCategories;
  private Date lastUpdate;
}
