package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkAutoApprovedProductsDownloadResponse extends BulkDataResponse implements
    Serializable {
  private static final long serialVersionUID = 36008476817027903L;
  private List<AutoApprovedProductsResponse> autoApprovedProductsResponses = new ArrayList<>();
}
