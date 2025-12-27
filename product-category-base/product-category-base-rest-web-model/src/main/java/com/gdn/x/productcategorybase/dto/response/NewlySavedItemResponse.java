package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class NewlySavedItemResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -6119919013736435355L;

  private String itemCode;
  private String productItemId;
  private String generatedItemName;
  private String mainImageUrl;
  private double length;
  private double width;
  private double height;
  private double weight;
  private double shippingWeight;
  private Integer dangerousGoodsLevel;

}