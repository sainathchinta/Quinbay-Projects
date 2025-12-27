package com.gdn.mta.bulk.models.download.responsedata;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class PartialAllDownloadResponse {
  private boolean partialDownload;
}
