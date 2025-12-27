package com.gdn.partners.pcu.master.client.model;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class DimensionResponse extends BaseResponse {

  private static final long serialVersionUID = 7191261844063773901L;
  private String name;
  private String dimensionCode;
  private String dimensionType;
  private byte[] description;
  private String nameEnglish;
  private byte[] descriptionEnglish;
  private String valueType;
  private String example;
  private String dsAttributeName;
}
