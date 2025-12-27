package com.gdn.mta.bulk.dto.product;

import java.util.HashMap;
import java.util.Map;

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
public class BulkVideoDownloadRequestEventModel extends GdnBaseDomainEventModel {

  private String originalUrl;
  private String ownerCode;
  private String clientId;
  private String source;
  private Map<String, String> additionalFields = new HashMap<>();
}
