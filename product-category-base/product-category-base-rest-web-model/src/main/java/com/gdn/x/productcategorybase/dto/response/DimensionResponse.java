package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

@EqualsAndHashCode(callSuper = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class DimensionResponse extends BaseDTOResponse {

  private static final long serialVersionUID = -6493828283891136428L;
  private String name;
  private String dimensionCode;
  private String dimensionType;
  private byte[] description;
  private String nameEnglish;
  private byte[] descriptionEnglish;
  private String example;
  private String dsAttributeName;
}
