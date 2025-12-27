package com.gdn.partners.pcu.external.web.model.response;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class TemplateDownloadFilePathWebResponse {
  Map<String, String> internalTemplateDownloadFilePathMap = new HashMap<>();
}
