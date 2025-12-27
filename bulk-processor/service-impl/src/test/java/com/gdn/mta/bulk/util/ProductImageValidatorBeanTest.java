package com.gdn.mta.bulk.util;

import java.io.File;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.SystemParameter;

public class ProductImageValidatorBeanTest {
  
  @Mock
  private SystemParameter sysparam;

  @Mock
  private File mockImageFile;
  
  @InjectMocks
  private ProductImageValidatorBean imageValidator;

  private static final int IMAGE_MAX_SIZE = 1048576;
  
  ClassLoader classLoader = getClass().getClassLoader();
  
  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    Mockito.when(sysparam.getImageMaxSize()).thenReturn(IMAGE_MAX_SIZE);
  }

  @Test
  public void imageValid_validateImages() {
    File imageFile = new File(classLoader.getResource("CreateProductV2/valid-img.png").getFile());
    imageValidator.validateImages(imageFile);
  }
  
  @Test
  public void imageFileSizeMoreThanMaxAllowed_validateImages_throwsException() {
    Mockito.when(mockImageFile.length()).thenReturn(4194304L);
    Assertions.assertThrows(RuntimeException.class,
        () -> imageValidator.validateImages(mockImageFile));
  }

  @Test
  public void nullImageFile_validateImages_throwsException() {
    Assertions.assertThrows(RuntimeException.class, () -> imageValidator.validateImages(null));
  }
}
