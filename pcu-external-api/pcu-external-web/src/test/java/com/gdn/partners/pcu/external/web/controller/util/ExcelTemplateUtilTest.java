package com.gdn.partners.pcu.external.web.controller.util;

import com.gdn.partners.pcu.external.model.Constants;
import org.apache.commons.lang.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;

public class ExcelTemplateUtilTest {

  @Mock
  private MultipartFile multipartFile;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(multipartFile);
  }

  @Test
  public void updateSubjectToVatTemplate() throws Exception {
    Mockito.when(multipartFile.getOriginalFilename()).thenReturn("123.xlsx");
    ExcelTemplateUtil.updateSubjectToVatTemplate(multipartFile, Constants.DATA_BASE_DIR, StringUtils.EMPTY);
    Mockito.verify(multipartFile, Mockito.times(3)).getOriginalFilename();
    Mockito.verify(multipartFile).transferTo((File) Mockito.any());
  }
}