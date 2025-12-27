package com.gdn.x.product.rest.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SizeChartResponse extends BaseResponse {

  private static final long serialVersionUID = -4604566527725048256L;
  private String sizeChartCode;
  private String name;
  private String brand;
  private String brandCode;
  private String sizeAttributeCode;
  private String sizeAttributeName;
  private List<String> selectedValueTypes = new ArrayList<>();
  private List<String> selectedDimensionCodes = new ArrayList<>();
  private String gender;
  private String unit;
  private String fitTip;
  private String businessPartnerCode;
  private String attributeImageUrl;
  private List<SizeChartDataRow> sizeChartRows = new ArrayList<>();
}
