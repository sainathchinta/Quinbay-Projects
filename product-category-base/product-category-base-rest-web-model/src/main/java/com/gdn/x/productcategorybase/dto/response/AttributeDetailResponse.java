package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeDetailResponse extends BaseResponse{ 
  private static final long serialVersionUID = 8059328856423135708L;
  private String attributeCode;
  private String attributeType;
  private String name;
  private boolean isBasicView;
  private boolean isSkuValue;
  private boolean mandatory;
  private boolean screeningMandatory;
  private boolean variantCreatingUI;
  private boolean variantCreation;

}
