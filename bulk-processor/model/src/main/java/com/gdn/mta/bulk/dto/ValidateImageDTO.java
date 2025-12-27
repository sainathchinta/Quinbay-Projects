package com.gdn.mta.bulk.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ValidateImageDTO {

  private Map.Entry<String, String> image;
  private URLConnection connection;
  private String bulkProcessCode;
  private Set<String> urlImagesWithInvalidExtension = new HashSet<>();
  private List<String> images = new ArrayList<>();
  private BulkUploadErrorCounter bulkUploadErrorCounter;
  private StringBuilder validationErrorMessage;
  private boolean isInternationalMerchant;
  private boolean useHttpConnectionForImageDownload;
  private String allowedImageTypes;
}
