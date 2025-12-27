package com.gdn.mta.bulk.dto.product;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class BulkBasicInfoVideoDownloadResponseModel extends GdnBaseDomainEventModel {

  private String originalUrl;
  private String videoId;
  private String sourceUrl;
  private String errorMessage;
  private String coverImagePath;
  private String videoName;
  private String clientId;
  private Map<String, String> additionalFields = new HashMap<>();
  private String errorCode;
}
