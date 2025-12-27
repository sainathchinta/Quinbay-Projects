package com.gda.mta.product.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ImageQcHashCodeAndLocationPathRequest implements Serializable {

  private static final long serialVersionUID = -7869113729579013719L;
  private String hashCode;
  private String locationPath;

}
