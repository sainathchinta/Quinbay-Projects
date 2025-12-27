package com.gdn.micro.graphics.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.web.model.GraphicDimension;

public class ConverterUtilTest {

  private static final Integer WIDTH = 10;
  private static final Integer HEIGHT = 20;
  private static final Double QUALITY = 6.0;

  private CustomGraphicsSettings customGraphicsSettings;
  private BulkResizeImageRequest bulkResizeImageRequest;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    customGraphicsSettings = new CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new GraphicDimension());
    customGraphicsSettings.setHeight(HEIGHT);
    customGraphicsSettings.setWidth(WIDTH);
    customGraphicsSettings.setQuality(QUALITY);
    bulkResizeImageRequest = new BulkResizeImageRequest();
    bulkResizeImageRequest.setCustomGraphicsSettings(customGraphicsSettings);
  }

  @Test
  void getCustomGraphicsSettingsTest() {
    com.gdn.micro.graphics.model.CustomGraphicsSettings settings =
        ConverterUtil.getCustomGraphicsSettings(bulkResizeImageRequest);
    Assertions.assertEquals(HEIGHT, Integer.valueOf(settings.getHeight()));
    Assertions.assertEquals(WIDTH, Integer.valueOf(settings.getWidth()));
    Assertions.assertEquals(QUALITY, Double.valueOf(settings.getQuality()));
  }

}
