package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
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
public class SizeChartResponse extends BaseResponse {

  private static final long serialVersionUID = -7729563567212448750L;
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
