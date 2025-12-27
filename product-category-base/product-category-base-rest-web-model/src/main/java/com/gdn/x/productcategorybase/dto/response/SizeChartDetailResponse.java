package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.request.SizeChartDataRow;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

import java.util.ArrayList;
import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class SizeChartDetailResponse extends BaseDTOResponse {
  private static final long serialVersionUID = 2104751821854639116L;
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
  private byte[] descriptionEnglish;
  private byte[] description;
}
