package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class DimensionRequest implements Serializable {
  private static final long serialVersionUID = 8654938716160964438L;

  private String dimensionCode;
  private String name;
  private String nameEnglish;
  private String dsAttributeName;
  private byte[] description;
  private byte[] descriptionEnglish;
  private String example;
}
