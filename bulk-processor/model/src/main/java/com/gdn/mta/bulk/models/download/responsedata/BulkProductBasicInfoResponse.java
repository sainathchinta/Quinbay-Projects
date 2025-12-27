package com.gdn.mta.bulk.models.download.responsedata;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkProductBasicInfoResponse extends BulkDataResponse implements Serializable {

  private static final long serialVersionUID = 164887125135544285L;

  private List<List<String>> productContentList = new ArrayList<>();
  private boolean businessPartnerO2O;
  private Map<String, String> exceptionMap = new HashMap<>();
  private Map<String, Boolean> privilegedMap = new HashMap<>();
}
