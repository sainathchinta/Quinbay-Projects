package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryDataResponse implements Serializable {
  private static final long serialVersionUID = -1161052721641354494L;
  private String categoryCode;
  private String name;
  private String nameEnglish;
  private boolean active;
  private int level;
}
