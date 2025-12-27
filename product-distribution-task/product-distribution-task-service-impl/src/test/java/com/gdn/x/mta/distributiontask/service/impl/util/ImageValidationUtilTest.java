package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;

import com.gdn.x.mta.distributiontask.model.ProductImage;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
public class ImageValidationUtilTest {

  private static final String IMAGE_1 = "1.png";
  private static final String IMAGE_2 = "2.png";
  private static final boolean MAIN_IMAGE = true;
  private static final int SEQUENCE = 0;
  private static final boolean MARK_FOR_DELETE = false;
  private static String IMAGE_SOURCE_DIRECTORY = "images";
  private static String IMAGE_FINAL_DIRECTORY = "images1";

  @InjectMocks
  private ImageValidationUtil imageValidationUtil;

  private List<ProductImage> productImages;
  private File resourceDirectory;
  private File resourceDirectoryFinal;

  @BeforeEach
  public void setUp() throws Exception {
    ProductImage productImage = new ProductImage();
    productImage.setLocationPath(IMAGE_1);
    productImage.setMainImage(MAIN_IMAGE);
    productImage.setSequence(SEQUENCE);
    productImage.setMarkForDelete(MARK_FOR_DELETE);
    ProductImage productImage1 = new ProductImage();
    productImage1.setLocationPath(IMAGE_1);
    productImage1.setMainImage(MAIN_IMAGE);
    productImage1.setSequence(SEQUENCE);
    productImage1.setActive(true);
    productImage1.setMarkForDelete(MARK_FOR_DELETE);
    productImages = new ArrayList<>();
    productImages.add(productImage);
    resourceDirectory = new File(IMAGE_SOURCE_DIRECTORY);
    resourceDirectoryFinal = new File(IMAGE_FINAL_DIRECTORY);
    File imageFile = new File(resourceDirectory + "/" + IMAGE_1);
    File imageFileFinal = new File(resourceDirectoryFinal + "/" + IMAGE_1);
    FileUtils.touch(imageFile);
    FileUtils.touch(imageFileFinal);
  }

  @Test
   void validateProductImagesTest() throws Exception {
    this.imageValidationUtil.validateProductImages(productImages, resourceDirectory.getAbsolutePath(),
        resourceDirectoryFinal.getAbsolutePath());
  }

  @Test
   void validateProductImagesWithExceptionTest() throws Exception {
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    productImage2.setMainImage(MAIN_IMAGE);
    productImage2.setSequence(SEQUENCE);
    productImage2.setMarkForDelete(MARK_FOR_DELETE);
    productImages.add(productImage2);
    Assertions.assertThrows(Exception.class,
      () -> this.imageValidationUtil.validateProductImages(productImages, resourceDirectory.getAbsolutePath(),
        resourceDirectoryFinal.getAbsolutePath()));
  }

  @Test
   void validateProductImagesActiveWithExceptionTest() throws Exception {
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    productImage2.setMainImage(MAIN_IMAGE);
    productImage2.setSequence(SEQUENCE);
    productImage2.setActive(true);
    productImage2.setMarkForDelete(MARK_FOR_DELETE);
    productImages.add(productImage2);
    Assertions.assertThrows(Exception.class,
      () -> this.imageValidationUtil.validateProductImages(productImages, resourceDirectory.getAbsolutePath(),
        resourceDirectoryFinal.getAbsolutePath()));
  }

  @AfterEach
  public void tearDown() throws Exception {
    FileUtils.deleteDirectory(resourceDirectory);
    FileUtils.deleteDirectory(resourceDirectoryFinal);
  }
}
