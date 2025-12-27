package com.gdn.micro.graphics.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ResizeImageScalingModel extends GdnBaseDomainEventModel {
  private List<ImagePaths> imagePathsList = new ArrayList<>();
  private String clientId;
  private String requestId;
  private String storeId;
  private String uniqueIdentifier;
}
