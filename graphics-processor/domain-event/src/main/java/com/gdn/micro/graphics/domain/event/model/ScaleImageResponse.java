package com.gdn.micro.graphics.domain.event.model;

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
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ScaleImageResponse extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -7490097360404174055L;

  private String imagePathLocation;
  private String hashCode;
  private boolean isActive;
  private String clientId;
  private boolean success;
  private String errorMessage;
  private boolean commonImage;
}
