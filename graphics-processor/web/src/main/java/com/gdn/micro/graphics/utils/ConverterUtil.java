package com.gdn.micro.graphics.utils;

import java.util.ArrayList;
import java.util.List;

import org.springframework.web.bind.annotation.RequestBody;

import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDimension;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.ImageRequest;

public class ConverterUtil {
  public static final String REGEX_FOR_SPLIT_ON_LAST_DOT = "\\.(?=[^.]*$)";
  private static final String PERMITTED_CHARS_EXCLUDING_DOT = "[^a-zA-Z0-9-]";
  private static final String JPG_EXTENSION = ".jpg";
  private static final String WEBP_EXTENSION = ".webp";
  private static final String UNDERSCORE = "_";

  public static CustomGraphicsSettings getCustomGraphicsSettings(@RequestBody BulkResizeImageRequest request) {
    CustomGraphicsSettings customGraphicsSettings = new CustomGraphicsSettings();
    GraphicDimension graphicDimension = new GraphicDimension();
    customGraphicsSettings.setQuality((int) request.getCustomGraphicsSettings().getQuality());
    graphicDimension.setWidth(request.getCustomGraphicsSettings().getDimession().getWidth());
    graphicDimension.setHeight(request.getCustomGraphicsSettings().getDimession().getHeight());
    customGraphicsSettings.setDimension(graphicDimension);
    return customGraphicsSettings;
  }

  public static void getImageNameWithExtension(StringBuilder nameWithoutExtension, boolean webpConversionEnabled) {
    if (webpConversionEnabled) {
      nameWithoutExtension.append(WEBP_EXTENSION);
    } else {
      nameWithoutExtension.append(JPG_EXTENSION);
    }
  }

  public static List<GraphicImageDetail> getGraphicImageDetails(BulkResizeImageRequest request, String prefix,
      boolean webpConversionEnabled)
      throws Exception {
    List<GraphicImageDetail> graphicImageDetails = new ArrayList<>();
    for (ImageRequest imageRequest : request.getImageRequests()) {
      CustomGraphicsSettings customGraphicsSettings = getCustomGraphicsSettings(request);
      String imageNameWithoutExtension = imageRequest.getImageName().split(REGEX_FOR_SPLIT_ON_LAST_DOT)[0];
      StringBuilder newImageName =
          new StringBuilder(imageNameWithoutExtension.replaceAll(PERMITTED_CHARS_EXCLUDING_DOT, UNDERSCORE));
      ConverterUtil.getImageNameWithExtension(newImageName, webpConversionEnabled);
      String generatedImageLocation =
          GraphicsProcessorHelper.getResizedImagePath(request.getGroupCode(), newImageName.toString(), prefix);
      GraphicImageDetail graphicImageDetail =
          new GraphicImageDetail(imageRequest.getHashCode(), imageRequest.getAbsoluteImagePath(),
              generatedImageLocation, customGraphicsSettings, prefix, request.getGroupCode());
      graphicImageDetail.setCommonImage(imageRequest.isCommonImage());
      graphicImageDetails.add(graphicImageDetail);
    }
    return graphicImageDetails;
  }
}

