package com.gdn.x.productcategorybase.domain.event.model;

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
public class RestrictedKeywordActivityHistory implements Serializable {
  private static final long serialVersionUID = 6908302977151172613L;
  private String activity;
  private String oldValue;
  private String newValue;
}
