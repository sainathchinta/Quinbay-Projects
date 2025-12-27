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
public class OldAndNewPathDomainEventModel implements Serializable {
  private static final long serialVersionUID = -7400748270226305594L;
  private String oldPath;
  private String newPath;
}
