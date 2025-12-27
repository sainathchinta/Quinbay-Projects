package com.gdn.partners.pcu.master.client.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@JsonInclude
public class SizeChartRequest implements Serializable {

  private static final long serialVersionUID = 2220901212573679931L;
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
  private List<SizeChartDataRow> sizeChartRows = new ArrayList<>();
}
