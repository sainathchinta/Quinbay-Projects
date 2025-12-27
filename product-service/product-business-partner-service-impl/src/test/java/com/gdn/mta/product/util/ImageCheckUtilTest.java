package com.gdn.mta.product.util;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.mta.product.service.util.ImageCheckService;
import com.gdn.x.productcategorybase.dto.Image;
import org.mockito.InjectMocks;

public class ImageCheckUtilTest {

  private static final String SOURCE_DIRECTORY = "source_directory";
  private static final String FULL_IMAGE_SOURCE_DIRECTORY = "full_image_source_directory";
  private static final String LOCATION_PATH = "locationPath";
  private static final String LOCATION_PATH_1 = "locationPath1";

  private ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
  private Image image = new Image();
  private Image image1 = new Image();

  @BeforeEach
  public void init() {
    image.setMarkForDelete(false);
    image.setActive(true);
    image.setLocationPath(LOCATION_PATH);

    image1.setMarkForDelete(true);
    image1.setActive(false);
    image1.setLocationPath(LOCATION_PATH_1);

    productItemCreationRequest.setImages(Arrays.asList(image, image1));
  }
}
