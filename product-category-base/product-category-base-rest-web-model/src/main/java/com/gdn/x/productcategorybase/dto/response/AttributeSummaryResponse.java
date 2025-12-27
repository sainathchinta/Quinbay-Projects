package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class AttributeSummaryResponse extends BaseResponse{
  private static final long serialVersionUID = 7886451165912669210L;
  private String attributeCode;
  private String attributeType;
  private String name;
  private String englishName;
  private boolean skuValue;
  private boolean basicView;
  private List<String> options;
  private boolean variantCreation;
  private boolean variantCreatingUi;
  private boolean mandatory;
}
