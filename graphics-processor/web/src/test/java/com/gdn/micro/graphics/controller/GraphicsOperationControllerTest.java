package com.gdn.micro.graphics.controller;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.gdn.micro.graphics.web.model.FullImageUploadRequest;
import com.gdn.micro.graphics.web.model.MediumImageUploadRequest;
import com.gdn.micro.graphics.web.model.ThumbNailImageUploadRequest;
import com.gdn.micro.graphics.web.model.XgpImageScaleRequest;
import jakarta.servlet.ServletException;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.CoreMatchers;
import org.hamcrest.core.StringEndsWith;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.consul.ConsulClientContext;
import com.gdn.common.base.mapper.impl.OrikaMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDimension;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.model.IdentifyImageResult;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessService;
import com.gdn.micro.graphics.service.AsyncGraphicsProcessorServiceWrapper;
import com.gdn.micro.graphics.service.FileStorageService;
import com.gdn.micro.graphics.service.GraphicsProcessorService;
import com.gdn.micro.graphics.service.enums.TargetType;
import com.gdn.micro.graphics.web.helper.ApiPath;
import com.gdn.micro.graphics.web.model.BulkImagesProcessRequest;
import com.gdn.micro.graphics.web.model.BulkResizeImageRequest;
import com.gdn.micro.graphics.web.model.ImageRequest;
import com.gdn.micro.graphics.web.model.RemoveImageRequest;
import com.gdn.micro.graphics.web.model.ScaleEditedImageRequest;
import com.gdn.micro.graphics.web.model.ScaleImageRequest;
import com.gdn.micro.graphics.service.ImagePathConfiguration;

import ma.glasnost.orika.MapperFactory;
import ma.glasnost.orika.impl.DefaultMapperFactory;

import static org.mockito.ArgumentMatchers.eq;

public class GraphicsOperationControllerTest {

  // private static final String IMAGE_JPEG = "image/jpeg";
  // private static final String ORIGINAL_FILENAME = "original_filename";
  // private static String MOCK_PATH = "mocking";
  private static final ObjectMapper MAPPER = new ObjectMapper();

  private static final String CHANNEL_ID = "channelid";
  private static final String REQUEST_ID = "requestid";
  private static final String CLIENT_ID = "local";
  private static final String RESIZE = "resize";
  private static final String HOTEL_CLIENT_ID = "hotel";
  private static final String HOTEL_REMOVE = "hotel.remove";
  private static final String USER_NAME = "username";
  private static final String STORE_ID = "10001";
  private static final String IMAGE_PATH = "imagePath";

  private static final String IMAGE_NAME = "imageName";
  private static final String IMAGE_NAME_WITH_SPECIAL_CHARS = "imageName.123.jpg";
  private static final String DESTINATION_PATH = "d:\\tmp\\graphics-processor\\result/GROUP_CODE/resize"
      + "/imageName_123.jpg";
  private static final String DEFAULT_GROUP_CODE = "GROUP_CODE";
  private static final String HASH_CODE = "hashcode";
  private static final String RANDOM_SEED = "2282901963617323";
  private static final String VALUE =
      "56FBC5ADA29755DB90A0E804C8DAB13B410F6AA1F8F2B3C3E2A9EE32EC3A103B34225581041474BE69E8153ED3C6337745ED0C272B1A54D5E84A5F9D2D4D2A66";
  private static final String REMOVE_PATH = "hotel.remove";
  private static final String REMOVE_PATH_1 = "hotel";
  private static final String FILE_PATH = "hotel/full/537/Hotel-H-AXELL-sideImage-large-17.jpg";
  private static final String DIRECTORY_VALUE =
      "56FBC5ADA29755DB90A0E804C8DAB13B410F6AA1F8F2B3C3E2A9EE32EC3A103B34225581041474BE69E8153ED3C63377";
  private static final String DIRECTORY_PATH = "hotel/full/537/Hotel-H-AXELL-sideImage-large-17";
  private static final String DIRECTORY_PATH_IN_FOLDER = "hotel/full/537/Hotel-H-AXELL-sideImage-large-17/image.jpg";

  private byte[] imageContent;

  private MockMvc mockMvc;

  @InjectMocks
  private GraphicsOperationController controller;

  @Mock
  private GraphicsProcessorService service;

  @Mock
  private AsyncGraphicsProcessorServiceWrapper asyncService;

  @Mock
  private AsyncGraphicsProcessService asyncGraphicsProcessService;

  @Mock
  private ConsulClientContext consulClientContext;

  @Mock
  private OrikaMapper orikaMapper;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private ImagePathConfiguration imagePathConfiguration;

  @Captor
  private ArgumentCaptor<List<GraphicImageDetail>> listArgumentCaptor;

  private MockMultipartFile multipartFile;
  private RemoveImageRequest removeImageRequest;

  @BeforeEach
  public void initialize() throws Exception {
    MockitoAnnotations.initMocks(this);
    DefaultMapperFactory.Builder builder = new DefaultMapperFactory.Builder();
    MapperFactory factory = builder.build();
    Mockito.when(imagePathConfiguration.getLocationPrefix(CLIENT_ID)).thenReturn("d:\\tmp\\graphics-processor\\result");
    mockMvc = MockMvcBuilders.standaloneSetup(this.controller).build();

    imageContent = new byte[]{-1, -40, -20, -10};

    removeImageRequest = new RemoveImageRequest();
    removeImageRequest.setValue(VALUE);
    removeImageRequest.setRandomSeed(RANDOM_SEED);
    ReflectionTestUtils.setField(controller, "sourcePath", "/filestore/mta/images/source");
  }

  @AfterEach
  public void postTest() throws IOException {
    Mockito.verifyNoMoreInteractions(service);
    Mockito.verifyNoMoreInteractions(asyncService);
    Mockito.verifyNoMoreInteractions(imagePathConfiguration);
    FileUtils.deleteDirectory(new File(REMOVE_PATH));
    FileUtils.deleteDirectory(new File(REMOVE_PATH_1));
  }

  private File mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, imageContent);
    return file;
  }

  private File mockFolder(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, imageContent);
    file.mkdir();
    return file;
  }

  @Test
  void test(){

  }

  @Test
  void testConvertImage() throws Exception {
    multipartFile =
        new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
    String targetTypeListStr = "JPG";
    String fileNameNoExt = "sample-result";

    Mockito.doNothing().when(asyncService)
        .convert(Mockito.any(TargetType.class), Mockito.any(File.class), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
            Mockito.anyString(), eq(CLIENT_ID), eq(REQUEST_ID));
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    //matcher

    List matchers = new ArrayList();
    matchers.add(new StringEndsWith(true, "_"+IMAGE_NAME+".jpg"));
    this.mockMvc
    .perform(MockMvcRequestBuilders.multipart(ApiPath.CONVERT_OPERATION_PATH)
        .file(multipartFile).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
        .param("userName", USER_NAME).param("imageName", IMAGE_NAME).param("settings", settings)
        .param("targetTypeList",targetTypeListStr).param("fileNameNoExt",fileNameNoExt)
        .accept(MediaType.APPLICATION_JSON))
    .andExpect(MockMvcResultMatchers.status().isOk())
    .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
    .andExpect(MockMvcResultMatchers.jsonPath("$.value.resultMap.jpg[0].generatedImageLocation",
        CoreMatchers.allOf(matchers)))
    .andExpect(MockMvcResultMatchers.jsonPath("$.value.resultMap.jpg[0].imageName",
        CoreMatchers.allOf(matchers)));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    Mockito.verify(asyncService)
        .convert(Mockito.any(TargetType.class), Mockito.any(File.class), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
            Mockito.anyString(), eq(CLIENT_ID), eq(REQUEST_ID));
  }

  @Test
  void testConvertImage_fail() throws Exception {
    multipartFile = new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
    String targetTypeListStr = "JPG";
    String fileNameNoExt = "sample-result";

    Mockito.doThrow(ApplicationRuntimeException.class).when(asyncService)
        .convert(Mockito.any(TargetType.class), Mockito.any(File.class), Mockito.anyString(),
            Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
            Mockito.anyString(), eq(CLIENT_ID), eq(REQUEST_ID));
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    //matcher

    List matchers = new ArrayList();
    matchers.add(new StringEndsWith(true, "_" + IMAGE_NAME + ".jpg"));
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(ApiPath.CONVERT_OPERATION_PATH).file(multipartFile)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
          .param("clientId", CLIENT_ID).param("userName", USER_NAME).param("imageName", IMAGE_NAME)
          .param("settings", settings).param("targetTypeList", targetTypeListStr).param("fileNameNoExt", fileNameNoExt)
          .accept(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true))).andExpect(
          MockMvcResultMatchers
              .jsonPath("$.value.resultMap.jpg[0].generatedImageLocation", CoreMatchers.allOf(matchers))).andExpect(
          MockMvcResultMatchers.jsonPath("$.value.resultMap.jpg[0].imageName", CoreMatchers.allOf(matchers)));
    } catch (Exception e) {
      Assertions.assertEquals(ServletException.class, e.getClass());
    } finally {
      Mockito.verify(asyncService)
          .convert(Mockito.any(TargetType.class), Mockito.any(File.class), Mockito.anyString(),
              Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString(),
              Mockito.anyString(), eq(CLIENT_ID), eq(REQUEST_ID));
      Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    }
  }

  @Test
  void testDisplayImageNotFound() throws Exception {
    this.mockMvc
    .perform(MockMvcRequestBuilders.get(ApiPath.DISPLAY_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
        .param("username", USER_NAME).param("imagePath", IMAGE_NAME))
    .andExpect(MockMvcResultMatchers.status().isNotFound());
  }

  @Test
  void testDisplayImageNotFoundContentNotNull() throws Exception {
    Mockito.when(fileStorageService.gcsToFileForXRMA(IMAGE_NAME,CLIENT_ID)).thenReturn(new byte[10]);
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(ApiPath.DISPLAY_PATH)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
            .param("username", USER_NAME).param("imagePath", IMAGE_NAME));
  }
  @Test
  void testIdentityImage() throws Exception {
    IdentifyImageResult result = new IdentifyImageResult();
    //String resultString = "Geometry: 100x100\nJPEG-Quality:80";
    String resultString = "JPEG,80,100,100";
    result.consumeOutput(IOUtils.toInputStream(resultString, "UTF-8"));
    multipartFile =
        new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
    Mockito.when(service.getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString())).thenReturn(result);
    this.mockMvc
    .perform(MockMvcRequestBuilders.multipart(ApiPath.IDENTIFY_OPERATION_PATH)
        .file(multipartFile).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
        .param("userName", USER_NAME).accept(MediaType.APPLICATION_JSON))
    .andExpect(MockMvcResultMatchers.status().isOk())
    .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(service).getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString());
  }

  @Test
  void testScaleImage() throws Exception {
    multipartFile =
        new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
    File file = mockFile("target/sample1.jpg");
    Mockito.doNothing().when(asyncService).scale(Mockito.any(File.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    //matcher
    List matchers = new ArrayList();
    matchers.add(new StringEndsWith(true, IMAGE_NAME+".jpg"));

    Mockito.when(fileStorageService.uploadToGcsRMA(Mockito.anyString())).thenReturn(false);
    Mockito.doNothing().when(service)
        .uploadFileAndGenerateCommandRequestForScaling(Mockito.anyList(), Mockito.anyList(),
            Mockito.anyList(), Mockito.anyString(), Mockito.any(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean());

    this.mockMvc
    .perform(MockMvcRequestBuilders.multipart(ApiPath.SCALE_OPERATION_PATH)
        .file(multipartFile).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
        .param("userName", USER_NAME).param("imageName", IMAGE_NAME).param("settings", settings)
        .accept(MediaType.APPLICATION_JSON))
    .andExpect(MockMvcResultMatchers.status().isOk())
    .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(fileStorageService).uploadToGcsRMA(Mockito.anyString());
    Mockito.verify(service).uploadFileAndGenerateCommandRequestForScaling(Mockito.anyList(), Mockito.anyList(),
        Mockito.anyList(), Mockito.anyString(), Mockito.any(), Mockito.anyString(),
        eq(null), Mockito.anyBoolean());
    Mockito.verify(asyncService).scales(Mockito.any(ArrayList.class), Mockito.anyBoolean(), Mockito.anyString(), eq(REQUEST_ID));
  }
  
  @Test
  void testScaleImage_2() throws Exception {
    multipartFile =
        new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
    File file = mockFile("target/sample1.jpg");
    Mockito.doNothing().when(asyncService).scale(Mockito.any(File.class), Mockito.anyString(),
        Mockito.anyString(), Mockito.any(CustomGraphicsSettings.class), Mockito.anyString());
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    //matcher
    List matchers = new ArrayList();
    matchers.add(new StringEndsWith(true, IMAGE_NAME+".jpg"));

    Mockito.when(fileStorageService.uploadToGcsRMA(Mockito.anyString())).thenReturn(false);
    Mockito.doNothing().when(service)
        .uploadFileAndGenerateCommandRequestForScaling(Mockito.anyList(), Mockito.anyList(),
            Mockito.anyList(), Mockito.anyString(), Mockito.any(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyBoolean());


    this.mockMvc
        .perform(
            MockMvcRequestBuilders.multipart(ApiPath.SCALE_OPERATION_PATH).file(multipartFile)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
                .param("clientId", CLIENT_ID).param("userName", USER_NAME)
                .param("groupCode", GraphicsOperationControllerTest.DEFAULT_GROUP_CODE).param("imageName", IMAGE_NAME)
                .param("settings", settings).accept(MediaType.APPLICATION_JSON))
    .andExpect(MockMvcResultMatchers.status().isOk())
    .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(fileStorageService).uploadToGcsRMA(Mockito.anyString());
    Mockito.verify(service).uploadFileAndGenerateCommandRequestForScaling(Mockito.anyList(), Mockito.anyList(),
        Mockito.anyList(), Mockito.anyString(), Mockito.any(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(asyncService).scales(Mockito.any(ArrayList.class), Mockito.anyBoolean(), Mockito.anyString(), eq(REQUEST_ID));
  }

  @Test
  void testStoreImage() throws Exception {
    multipartFile =
        new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
    ImageResultDetail result = new ImageResultDetail();
    result.setSuccess(true);

    IdentifyImageResult imageResult = new IdentifyImageResult();
    String resultString = "Geometry: 100x100\nJPEG-Quality:80";
    imageResult.consumeOutput(IOUtils.toInputStream(resultString, "UTF-8"));

    Mockito.when(fileStorageService.getImageLocationPathPrefix(Mockito.anyString())).thenReturn("prefix");
    Mockito.when(fileStorageService.uploadToGcsOxford(Mockito.anyString())).thenReturn(false);
    Mockito.when(service.store(Mockito.any(InputStream.class), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean())).thenReturn(result);
    Mockito.when(service.getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString())).thenReturn(imageResult);

    this.mockMvc
    .perform(MockMvcRequestBuilders.multipart(ApiPath.STORE_OPERATION_PATH)
        .file(multipartFile).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
        .param("userName", USER_NAME).param("imageName", IMAGE_NAME)
        .accept(MediaType.APPLICATION_JSON))
    .andExpect(MockMvcResultMatchers.status().isOk())
    .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));

    Mockito.verify(service).store(Mockito.any(InputStream.class), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
    Mockito.verify(service).getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString());
    Mockito.verify(fileStorageService).getImageLocationPathPrefix(Mockito.anyString());
    Mockito.verify(fileStorageService).uploadToGcsOxford(Mockito.anyString());

  }

  @Test
  void storeExceptionTest() throws Exception {
    try {
      multipartFile = new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
      ImageResultDetail result = new ImageResultDetail();
      result.setSuccess(true);

      IdentifyImageResult imageResult = new IdentifyImageResult();
      String resultString = "Geometry: 100x100\nJPEG-Quality:80";
      imageResult.consumeOutput(IOUtils.toInputStream(resultString, "UTF-8"));

      Mockito.when(fileStorageService.getImageLocationPathPrefix(Mockito.anyString())).thenReturn("prefix");
      Mockito.when(fileStorageService.uploadToGcsOxford(Mockito.anyString())).thenReturn(false);
      Mockito.when(service.store(Mockito.any(InputStream.class), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean()))
          .thenThrow(new ApplicationException());
      Mockito.when(service.getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString())).thenReturn(imageResult);

      this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(ApiPath.STORE_OPERATION_PATH).file(multipartFile).param("storeId",
              STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param
              ("clientId", CLIENT_ID).param("userName", USER_NAME).param("imageName", IMAGE_NAME)
              .accept(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(service).store(Mockito.any(InputStream.class), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
      Mockito.verify(service).getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString());
      Mockito.verify(fileStorageService).getImageLocationPathPrefix(Mockito.anyString());
      Mockito.verify(fileStorageService).uploadToGcsOxford(Mockito.anyString());
    }
  }


  @Test
  void testStoreImage_fail() throws Exception {
    multipartFile = new MockMultipartFile("content", this.getClass().getResourceAsStream("/sample1.jpg"));
    ImageResultDetail result = new ImageResultDetail();
    result.setSuccess(true);

    IdentifyImageResult imageResult = new IdentifyImageResult();
    String resultString = "Geometry: 100x100\nJPEG-Quality:80";
    imageResult.consumeOutput(IOUtils.toInputStream(resultString, "UTF-8"));

    Mockito.when(fileStorageService.getImageLocationPathPrefix(Mockito.anyString())).thenReturn("prefix");
    Mockito.when(fileStorageService.uploadToGcsOxford(Mockito.anyString())).thenReturn(false);
    Mockito.when(service.store(Mockito.any(InputStream.class), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean()))
        .thenReturn(result);
    Mockito.when(service.getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString())).thenReturn(imageResult);

    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(ApiPath.STORE_OPERATION_PATH).file(multipartFile).param("storeId", STORE_ID)
              .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
              .param("userName", USER_NAME).param("imageName", IMAGE_NAME).accept(MediaType.APPLICATION_JSON))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (Exception e) {
      Assertions.assertEquals(ApplicationRuntimeException.class, e.getClass());
    } finally {
      Mockito.verify(service).store(Mockito.any(InputStream.class), Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.anyBoolean());
      Mockito.verify(service).getGraphicsProperty(Mockito.any(InputStream.class), Mockito.anyString());
      Mockito.verify(fileStorageService).getImageLocationPathPrefix(Mockito.anyString());
      Mockito.verify(fileStorageService).uploadToGcsOxford(Mockito.anyString());

    }
  }

  @Test
  void testScaleBulkImage() throws Exception {
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .scaleBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(settings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.SCALE_BULK_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    ;
    Mockito.verify(asyncGraphicsProcessService).scaleBulkImages(Mockito.any(List.class),
        eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }

  @Test
  void testScaleBulkWebpImage() throws Exception {
    ReflectionTestUtils.setField(controller, "webpConversionEnabled", true);
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .scaleBulkImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(settings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(ApiPath.SCALE_BULK_IMAGES_OPERATION_PATH).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
                .param("username", USER_NAME).content(requestBody).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    ;
    Mockito.verify(asyncGraphicsProcessService)
        .scaleBulkImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }

  @Test
  void testResizeBulkImage() throws Exception {
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .resizeBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_BULK_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    ;
    Mockito.verify(asyncGraphicsProcessService).resizeBulkImages(Mockito.any(List.class),
        eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }

  @Test
  void testResizeBulkWebpImage() throws Exception {
    ReflectionTestUtils.setField(controller, "webpConversionEnabled", true);
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .resizeBulkImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(ApiPath.RESIZE_BULK_IMAGES_OPERATION_PATH).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
                .param("username", USER_NAME).content(requestBody).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    ;
    Mockito.verify(asyncGraphicsProcessService)
        .resizeBulkImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }

  @Test
  void testResizeEditImage() throws Exception {
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .resizeEditedImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_EDITED_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    ;
    Mockito.verify(asyncGraphicsProcessService).resizeEditedImages(Mockito.any(List.class),
        eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }

  @Test
  void testResizeEditWebpImage() throws Exception {
    ReflectionTestUtils.setField(controller, "webpConversionEnabled", true);
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .resizeEditedImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(ApiPath.RESIZE_EDITED_IMAGES_OPERATION_PATH).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
                .param("username", USER_NAME).content(requestBody).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    ;
    Mockito.verify(asyncGraphicsProcessService)
        .resizeEditedImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }

  @Test
  void testResizeBulkImageWithSpecialChars() throws Exception {
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .scaleBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME_WITH_SPECIAL_CHARS);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_BULK_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    ;
    Mockito.verify(asyncGraphicsProcessService).resizeBulkImages(listArgumentCaptor.capture(),
        eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
    Assertions.assertEquals(DESTINATION_PATH, listArgumentCaptor.getValue().get(0).getDestinationPath());
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }


  @Test
  void testRemoveExceptionTest() throws Exception {
    Mockito.when(imagePathConfiguration.getLocationPrefix(HOTEL_REMOVE)).thenReturn(HOTEL_REMOVE);
    Mockito.when(imagePathConfiguration.getLocationPrefix(HOTEL_CLIENT_ID)).thenReturn(HOTEL_CLIENT_ID);
    String requestBody = MAPPER.writeValueAsString(removeImageRequest);
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.post(ApiPath.REMOVE_PATH).param("storeId", STORE_ID)
                  .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
                  .param("clientId", HOTEL_REMOVE).param("username", USER_NAME).content(requestBody)
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (ServletException ex) {
      Assertions.assertNotNull(ex);
    }
  }

  @Test
  void testRemoveDirectory() throws Exception {
    String requestBody = MAPPER.writeValueAsString(removeImageRequest);
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.post(ApiPath.REMOVE_PATH).param("storeId", STORE_ID)
                  .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
                  .param("clientId", "hotel").param("username", USER_NAME).content(requestBody)
                  .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (ServletException ex) {
      Assertions.assertNotNull(ex);
    }
    finally {
      Assertions.assertFalse(new File(DIRECTORY_PATH).exists());
    }
  }

  @Test
  void testRemoveDirectoryontContentNotNull() throws Exception {
    String requestBody = MAPPER.writeValueAsString(removeImageRequest);
    Mockito.when(fileStorageService.gcsRemoveForOxford(Mockito.anyString(),Mockito.anyString())).thenReturn(new byte[1]);
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(ApiPath.REMOVE_PATH).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("requestId", REQUEST_ID).param("clientId", "hotel").param("username", USER_NAME).content(requestBody)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE));
  }
  @Test
  void testScaleBulkImage_fail() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .scaleBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(settings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.SCALE_BULK_IMAGES_OPERATION_PATH)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
          .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    }  finally {
        Mockito.verify(asyncGraphicsProcessService).scaleBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
      Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
      }
  }

  @Test
  void testResizeBulkImage_fail() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .resizeBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);

    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_BULK_IMAGES_OPERATION_PATH)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
          .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
        Mockito.verify(asyncGraphicsProcessService).resizeBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
      Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    }
  }

  @Test
  void testResizeEditedImage_fail() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .resizeEditedImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);

    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_EDITED_IMAGES_OPERATION_PATH)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
          .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(asyncGraphicsProcessService).resizeEditedImages(Mockito.any(List.class),
          eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
      Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    }
  }

  @Test
  void testScaleBulkImage_withoutImageRequest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .scaleBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(false), eq(0));
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.SCALE_BULK_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
  }

  @Test
  void testResizeBulkImage_withoutImageRequest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .resizeBulkImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID), eq(0));
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_BULK_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
  }

  @Test
  void testEditedBulkImage_withoutImageRequest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .resizeEditedImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_EDITED_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
  }

  @Test
  void scaleEditedImagesTest() throws Exception {
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .scaleEditedImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE),
            eq(CLIENT_ID), Mockito.any(ScaleEditedImageRequest.class), Mockito.anyString());
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    ScaleEditedImageRequest request = new ScaleEditedImageRequest();
    request.setProductCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(settings);
    ScaleImageRequest imageRequest =
        ScaleImageRequest.builder().imagePathLocation(IMAGE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(false).build();
    ScaleImageRequest imageRequest1 =
        ScaleImageRequest.builder().imagePathLocation(IMAGE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(true).build();
    request.setImageRequests(Arrays.asList(imageRequest, imageRequest1));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(ApiPath.SCALE_EDITED_IMAGES_OPERATION_PATH).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
            .param("username", USER_NAME).content(requestBody).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(asyncGraphicsProcessService)
        .scaleEditedImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE),
            eq(CLIENT_ID), Mockito.any(ScaleEditedImageRequest.class), Mockito.anyString());
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
  }

  @Test
  void scaleEditedImagesTest_Exception() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .scaleEditedImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE),
            eq(CLIENT_ID), Mockito.any(ScaleEditedImageRequest.class), Mockito.anyString());
    List<CustomGraphicsSettings> customGraphicsSettings = new ArrayList<>();
    customGraphicsSettings.add(new CustomGraphicsSettings(80, 80, new GraphicDimension(100, 100)));
    String settings = MAPPER.writeValueAsString(customGraphicsSettings);
    ScaleEditedImageRequest request = new ScaleEditedImageRequest();
    request.setProductCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(settings);
    ScaleImageRequest imageRequest =
        ScaleImageRequest.builder().imagePathLocation(IMAGE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(false).build();
    ScaleImageRequest imageRequest1 =
        ScaleImageRequest.builder().imagePathLocation(IMAGE_PATH).imageName(IMAGE_NAME).hashCode(HASH_CODE)
            .isActive(true).build();
    request.setImageRequests(Arrays.asList(imageRequest, imageRequest1));
    String requestBody = MAPPER.writeValueAsString(request);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.post(ApiPath.SCALE_EDITED_IMAGES_OPERATION_PATH).param("storeId", STORE_ID)
              .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
              .param("username", USER_NAME).content(requestBody).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(asyncGraphicsProcessService)
          .scaleEditedImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE),
              eq(CLIENT_ID), Mockito.any(ScaleEditedImageRequest.class), Mockito.anyString());
      Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    }
  }

  @Test
  void resizeRevisedImagesTest() throws Exception {
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .resizeRevisedImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    imageRequest.setCommonImage(true);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_REVISED_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(asyncGraphicsProcessService).resizeRevisedImages(listArgumentCaptor.capture(),
        eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    Assertions.assertTrue(listArgumentCaptor.getValue().get(0).isCommonImage());
  }

  @Test
  void resizeRevisedWebpImagesTest() throws Exception {
    ReflectionTestUtils.setField(controller, "webpConversionEnabled", true);
    Mockito.doNothing().when(asyncGraphicsProcessService)
        .resizeRevisedImages(Mockito.any(List.class), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    imageRequest.setCommonImage(true);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(ApiPath.RESIZE_REVISED_IMAGES_OPERATION_PATH).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID).param("clientId", CLIENT_ID)
                .param("username", USER_NAME).content(requestBody).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON_VALUE)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(asyncGraphicsProcessService)
        .resizeRevisedImages(listArgumentCaptor.capture(), eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    Assertions.assertTrue(listArgumentCaptor.getValue().get(0).isCommonImage());
  }

  @Test
  void resizeRevisedImagesTestExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .resizeRevisedImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    com.gdn.micro.graphics.web.model.CustomGraphicsSettings customGraphicsSettings =
        new com.gdn.micro.graphics.web.model.CustomGraphicsSettings();
    customGraphicsSettings.setDimession(new com.gdn.micro.graphics.web.model.GraphicDimension(800, 800));
    customGraphicsSettings.setQuality(60);
    BulkResizeImageRequest request = new BulkResizeImageRequest();
    request.setGroupCode(DEFAULT_GROUP_CODE);
    request.setCustomGraphicsSettings(customGraphicsSettings);
    ImageRequest imageRequest = new ImageRequest();
    imageRequest.setImageName(IMAGE_NAME);
    imageRequest.setAbsoluteImagePath(IMAGE_PATH);
    imageRequest.setHashCode(HASH_CODE);
    request.setImageRequests(Arrays.asList(imageRequest));
    String requestBody = MAPPER.writeValueAsString(request);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_REVISED_IMAGES_OPERATION_PATH)
          .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
          .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
          .andExpect(MockMvcResultMatchers.status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(asyncGraphicsProcessService).resizeRevisedImages(Mockito.any(List.class),
          eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
      Mockito.verify(imagePathConfiguration).getLocationPrefix(CLIENT_ID);
    }
  }

  @Test
  void resizeRevisedImagesTestExceptionTestEmptyRequestTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
        .resizeRevisedImages(Mockito.any(List.class),
            eq(DEFAULT_GROUP_CODE), eq(CLIENT_ID));
    BulkImagesProcessRequest request = new BulkImagesProcessRequest();
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.RESIZE_REVISED_IMAGES_OPERATION_PATH)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("requestId", REQUEST_ID)
        .param("clientId", CLIENT_ID).param("username", USER_NAME).content(requestBody)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
  }

  @Test
  void scaleActiveProductNewImagesTest() throws Exception {
    Mockito.doNothing().when(asyncGraphicsProcessService)
      .scaleActiveProductNewImages(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(XgpImageScaleRequest.class), Mockito.anyString());
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setFullImageUploadRequest(new FullImageUploadRequest());
    request.setMediumImageUploadRequest(new MediumImageUploadRequest());
    request.setThumbNailImageUploadRequest(new ThumbNailImageUploadRequest());
    String requestBody = MAPPER.writeValueAsString(request);
    this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.SCALE_ACTIVE_PRODUCT_NEW_IMAGES)
        .param("storeId", STORE_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .content(requestBody).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON_VALUE))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(asyncGraphicsProcessService)
      .scaleActiveProductNewImages(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(XgpImageScaleRequest.class), Mockito.anyString());
  }

  @Test
  void scaleActiveProductNewImagesExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(asyncGraphicsProcessService)
      .scaleActiveProductNewImages(Mockito.anyString(), Mockito.anyString(),
        Mockito.any(XgpImageScaleRequest.class), Mockito.anyString());
    XgpImageScaleRequest request = new XgpImageScaleRequest();
    request.setFullImageUploadRequest(new FullImageUploadRequest());
    request.setMediumImageUploadRequest(new MediumImageUploadRequest());
    request.setThumbNailImageUploadRequest(new ThumbNailImageUploadRequest());
    String requestBody = MAPPER.writeValueAsString(request);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(ApiPath.SCALE_ACTIVE_PRODUCT_NEW_IMAGES)
          .param("storeId", STORE_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
          .content(requestBody).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON_VALUE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (Exception ex) {
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(asyncGraphicsProcessService)
        .scaleActiveProductNewImages(Mockito.anyString(), Mockito.anyString(),
          Mockito.any(XgpImageScaleRequest.class), Mockito.anyString());
    }
  }
}
