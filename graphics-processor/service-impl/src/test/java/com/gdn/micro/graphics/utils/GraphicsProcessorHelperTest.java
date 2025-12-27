package com.gdn.micro.graphics.utils;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.imageio.ImageIO;

import com.gdn.common.exception.ApplicationRuntimeException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.micro.graphics.AvailableSize;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDimension;
import com.gdn.micro.graphics.service.enums.TargetType;


public class GraphicsProcessorHelperTest {

  private static final String PATH = "path/1.jpg";
  private static final String STRING_TO_MODULUS = "path/1.jpg";
  private static final Integer DPI = 10;
  private static final Integer QUALITY = 6;

  private CustomGraphicsSettings customGraphicsSettings;
  private List<CustomGraphicsSettings> customGraphicsSettingsList;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);

    customGraphicsSettings = new CustomGraphicsSettings(DPI, QUALITY, new GraphicDimension());
    customGraphicsSettingsList = new ArrayList<>();
    customGraphicsSettingsList.add(customGraphicsSettings);
  }

  @AfterEach
  public void tearDown() throws Exception {
    FileUtils.deleteDirectory(new File("path"));
  }

  @Test
  void targetTypeTest() {
    Assertions.assertEquals("jpg", TargetType.valueOf("JPG").toString());
    Assertions.assertEquals("bpg", TargetType.valueOf("BPG").toString());
    Assertions.assertTrue(TargetType.valueOf("JPG").equalsName("jpg"));
  }

  @Test
  void createDestinationDirectory() throws Exception {
    GraphicsProcessorHelper.createDestinationDirectory(PATH);
  }

  @Test
  void getFileSystemPath() throws Exception {
    GraphicsProcessorHelper.getFileSystemPath(STRING_TO_MODULUS, AvailableSize.ALL, ".jpg");
  }

  @Test
  void generateTemporaryFileFromStream() throws Exception {
    FileInputStream fileInputStream = new FileInputStream(mockFile(PATH));
    GraphicsProcessorHelper.generateTemporaryFileFromStream(fileInputStream, ".jpg");
  }

  @Test
  void generateTemporaryFileFromStreamTest() throws Exception {
    try {
      GraphicsProcessorHelper.generateTemporaryFileFromStream(null, ".jpg");
    } catch (ApplicationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void getFileSystemPath1() throws Exception {
    GraphicsProcessorHelper.getFileSystemPath("MTA_00001", PATH,  AvailableSize.ALL, ".jpg");
  }

  @Test
  void getFileSystemPathEmpty1() throws Exception {
    GraphicsProcessorHelper.getFileSystemPath(StringUtils.EMPTY, PATH, AvailableSize.ALL, ".jpg");
  }

  @Test
  void getResizedImagePath() throws Exception {
    GraphicsProcessorHelper.getResizedImagePath(StringUtils.EMPTY, PATH, ".jpg");
  }

  @Test
  void getSize() {
    GraphicsProcessorHelper.getSize(100);
  }


  @Test
  void initializeSettingsFromStringTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    List<CustomGraphicsSettings> settings = GraphicsProcessorHelper
        .initializeSettingsFromString(objectMapper.writeValueAsString(customGraphicsSettingsList), objectMapper);
    Assertions.assertEquals(DPI, Integer.valueOf(settings.get(0).getDpi()));
    Assertions.assertEquals(Double.valueOf(QUALITY), Double.valueOf(settings.get(0).getQuality()));
  }

  @Test
  void initializeSettingsFromStringExceptionTest() throws Exception {
    try {
      List<CustomGraphicsSettings> settings =
          GraphicsProcessorHelper.initializeSettingsFromString("", new ObjectMapper());
    } catch (ApplicationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void initializeSettingFromStringTest() throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    CustomGraphicsSettings settings = GraphicsProcessorHelper
        .initializeSettingFromString(objectMapper.writeValueAsString(customGraphicsSettings), objectMapper);
    Assertions.assertEquals(DPI, Integer.valueOf(settings.getDpi()));
    Assertions.assertEquals(Double.valueOf(QUALITY), Double.valueOf(settings.getQuality()));
  }

  @Test
  void initializeSettingFromStringExceptionTest() throws Exception {
    try {
      CustomGraphicsSettings settings =
          GraphicsProcessorHelper.initializeSettingFromString("", new ObjectMapper());
    } catch (ApplicationException ex){
      Assertions.assertNotNull(ex);
    }
  }

  private File mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    BufferedImage img = new BufferedImage(640, 320, BufferedImage.TYPE_INT_RGB);
    ImageIO.write(img, "jpg", file);
    return file;
  }
}