package com.gdn.mta.bulk.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.dto.TaggedProductFilterResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@AllArgsConstructor
@NoArgsConstructor
@Data
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadTaggedProductsResponse extends BulkDataResponse {
  private List<TaggedProductFilterResponse> taggedProductFilterResponseList = new ArrayList<>();
  private String emailAddress;
}
