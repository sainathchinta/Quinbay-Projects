package com.gdn.micro.graphics.domain.event.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ImageDownloadResponse {
  private String imageSourcePath;
  private String imageModifiedSourcePath;
}
