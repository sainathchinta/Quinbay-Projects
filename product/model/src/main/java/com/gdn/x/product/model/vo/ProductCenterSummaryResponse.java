package com.gdn.x.product.model.vo;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import java.util.Date;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductCenterSummaryResponse extends BaseResponse {
  private String productSku;
  private String productName;
  private String status;
  private MasterCategoryResponse masterCategory;
  private List<String> salesCategories;
  private Date lastUpdate;
}