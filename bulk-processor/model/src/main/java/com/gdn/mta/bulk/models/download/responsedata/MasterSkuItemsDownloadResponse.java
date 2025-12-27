package com.gdn.mta.bulk.models.download.responsedata;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSkuItemsDownloadResponse extends BulkDataResponse {

  private List<ItemSkuAndMasterSkuResponse> itemSkuAndMasterSkuResponseList = new ArrayList<>();
}
