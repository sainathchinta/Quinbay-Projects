package com.gdn.partners.pcu.external.web.model.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class BulkSelectedDownloadHeaders {

  private List<String> primaryHeaders = new ArrayList<>();
  private List<String> secondaryHeaders = new ArrayList<>();
  private List<String> tertiaryHeaders = new ArrayList<>();
  private List<String> quaternaryHeaders = new ArrayList<>();
  private int pickupPointIndex;
}