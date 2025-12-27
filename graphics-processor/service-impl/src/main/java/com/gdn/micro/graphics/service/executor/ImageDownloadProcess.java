package com.gdn.micro.graphics.service.executor;

import java.util.concurrent.Callable;

import com.gdn.micro.graphics.domain.event.model.ImageDownloadResponse;
import com.gdn.micro.graphics.service.FileStorageService;

import lombok.AllArgsConstructor;

@AllArgsConstructor
public class ImageDownloadProcess implements Callable<ImageDownloadResponse> {
  private final String sourcePath;
  private final String sourcePrefixPath;
  private final boolean isResize;
  private final boolean isEdited;
  private final FileStorageService fileStorageService;

  @Override
  public ImageDownloadResponse call() throws Exception {
    String modifiedSourcePath = fileStorageService.getLocalSourcePathOfFile(sourcePath, sourcePrefixPath, isResize,
        fileStorageService.getProductCodeFileName(sourcePath), isEdited);
    return new ImageDownloadResponse(sourcePath, modifiedSourcePath);
  }
}
