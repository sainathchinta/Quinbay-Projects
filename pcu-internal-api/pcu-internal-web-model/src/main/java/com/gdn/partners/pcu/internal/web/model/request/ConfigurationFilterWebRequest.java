package com.gdn.partners.pcu.internal.web.model.request;

import java.util.List;

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
public class ConfigurationFilterWebRequest {

  String reviewConfig;
  String categoryCode;
  String searchKey;
  String sortOrder = "desc";
  List<String> dataList;
}
