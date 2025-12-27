package com.gdn.micro.graphics.service.executor;

import java.util.concurrent.Callable;
import org.springframework.beans.BeanUtils;
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.service.GraphicsProcessorService;

/**
 * Created by Vishal on 10/06/18.
 */
public class AsyncImageProcessor implements Callable<ImageResponse>{

  private final String hashCode;
  private final String sourcePath;
  private final String destinationPath;
  private CustomGraphicsSettings customGraphicsSettings;
  private final String prefixPath;
  private final String clientId;
  private boolean resize;
  private boolean commonImage;
  private final GraphicsProcessorService graphicsProcessorService;
  private Boolean isEdited;

  public AsyncImageProcessor(String hashCode, String sourcePath, String destinationPath,
      CustomGraphicsSettings customGraphicsSettings, String prefixPath, String clientId, boolean resize,
      GraphicsProcessorService graphicsProcessorService, Boolean isEdited) {
    this.hashCode = hashCode;
    this.sourcePath = sourcePath;
    this.clientId = clientId;
    this.destinationPath = destinationPath;
    this.customGraphicsSettings = customGraphicsSettings;
    this.prefixPath = prefixPath;
    this.resize = resize;
    this.graphicsProcessorService = graphicsProcessorService;
    this.isEdited = isEdited;
  }

  public String getHashCode() {
    return hashCode;
  }

  public String getSourcePath() {
    return sourcePath;
  }



  public String getDestinationPath() {
    return destinationPath;
  }

  public CustomGraphicsSettings getCustomGraphicsSettings() {
    return customGraphicsSettings;
  }

  public String getPrefixPath() {
    return prefixPath;
  }

  public boolean isResize() {
    return resize;
  }

  public boolean isCommonImage() {
    return commonImage;
  }

  public void setCommonImage(boolean commonImage) {
    this.commonImage = commonImage;
  }

  public Boolean getEdited() {
    return isEdited;
  }

  public void setEdited(Boolean edited) {
    isEdited = edited;
  }

  @Override
  public ImageResponse call() throws Exception {
    ImageResponse response = new ImageResponse();
    response.setHashCode(getHashCode());
    ImageResultDetail imageResultDetail = graphicsProcessorService
        .scale(getSourcePath(), getDestinationPath(), getCustomGraphicsSettings(), getPrefixPath(), isResize(), isEdited);
    BeanUtils.copyProperties(imageResultDetail, response);
    response.setClientId(this.clientId);
    response.setCommonImage(this.commonImage);
    return response;
  }
}
