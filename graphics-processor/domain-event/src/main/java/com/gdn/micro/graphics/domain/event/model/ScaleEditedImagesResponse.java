package com.gdn.micro.graphics.domain.event.model;

import java.io.Serializable;
import java.util.List;

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
public class ScaleEditedImagesResponse extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -3863818719321014862L;

  private String productCode;
  private String storeId;
  private String username;
  private List<ScaleImageResponse> imageResponses;
}
