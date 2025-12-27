package com.gdn.x.productcategorybase.dto.request;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryAttributeUpdateRequest implements Serializable{

  private static final long serialVersionUID = 6791048330832112068L;
  private String attributeId;
  private Integer sequence;
  private boolean mainDefiningAttribute = false;
  private boolean usp = false;

}
