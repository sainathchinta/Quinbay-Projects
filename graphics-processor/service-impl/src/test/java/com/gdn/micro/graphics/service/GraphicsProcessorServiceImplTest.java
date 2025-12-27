package com.gdn.micro.graphics.service;

import java.io.File;
import java.io.FileInputStream;

import org.gm4java.engine.support.GMConnectionPoolConfig;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.CustomGraphicsSettings;
import com.gdn.micro.graphics.model.GraphicDimension;
import com.gdn.micro.graphics.model.IdentifyImageResult;
import com.gdn.micro.graphics.service.enums.TargetType;

/**
 * Created by Yudhi K. Surtan on 11/29/2015.
 */
public class GraphicsProcessorServiceImplTest {

//  private ClassLoader classLoader;
//
//  @InjectMocks
//  private GraphicsProcessorServiceImpl service = new GraphicsProcessorServiceImpl(new GMConnectionPoolConfig());;
//
//  @Before
//  public void initializeTest() {
//    MockitoAnnotations.initMocks(this);
//    classLoader = getClass().getClassLoader();  }
//
//  @Test
//  public void testIsGraphicsMethod() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    boolean graphics = service.isGraphics(imagePath + "/TestImage.jpg");
//    Assertions.assertTrue(graphics);
//  }
//
//  @Test
//  public void getGraphicsPropertyTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    IdentifyImageResult graphicsProperty =
//        service.getGraphicsProperty(new FileInputStream(new File(imagePath + "/TestImage.jpg")));
//    Assertions.assertTrue(graphicsProperty.isImage());
//    Assertions.assertEquals(Integer.valueOf(800), graphicsProperty.getHeight());
//  }
//
//  @Test
//  public void convertTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail testImage = service.convert(TargetType.JPG, imagePath + "/TestImage.jpg", imagePath, "TestImage",
//        new CustomGraphicsSettings(72, 75, new GraphicDimension(800, 800)),
//            imagePath);
//    Assertions.assertTrue(testImage.isSuccess());
//  }
//
//  @Test
//  public void scaleTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail scale = service.scale(imagePath + "/TestImage.jpg", imagePath + "/TestImage.jpg",
//        new CustomGraphicsSettings(72, 75, new GraphicDimension(800, 800)), imagePath, false);
//    Assertions.assertTrue(scale.isSuccess());
//  }
//
//  @Test
//  public void scaleResizeTrueTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail scale = service.scale(imagePath + "/TestImage.jpg", imagePath + "/TestImage.jpg",
//        new CustomGraphicsSettings(72, 75, new GraphicDimension(800, 800)), imagePath, true);
//    Assertions.assertTrue(scale.isSuccess());
//  }
//
//  @Test
//  public void scaleFullTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail scale = service.scaleFull(imagePath + "/TestImage.jpg", imagePath + "/TestImage.jpg", imagePath);
//    Assertions.assertTrue(scale.isSuccess());
//  }
//
//  @Test
//  public void scaleMediumTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail scale =
//        service.scaleMedium(imagePath + "/TestImage.jpg", imagePath + "/TestImage.jpg", imagePath);
//    Assertions.assertTrue(scale.isSuccess());
//  }
//
//  @Test
//  public void scaleThumbnailTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail scale =
//        service.scaleThumbnail(imagePath + "/TestImage.jpg", imagePath + "/TestImage.jpg", imagePath);
//    Assertions.assertTrue(scale.isSuccess());
//  }
//
//  @Test
//  public void storeTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail store =
//        service.store(new FileInputStream(new File(imagePath + "/TestImage.jpg")), imagePath + "/TestImage.jpg", imagePath);
//    Assertions.assertTrue(store.isSuccess());
//  }
//
//  @Test
//  public void stripMetadataAndAddInterlanceTest() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail scale =
//        service.stripMetadataAndAddInterlance(imagePath + "/TestImage.jpg", imagePath + "/TestImage.jpg", imagePath);
//    Assertions.assertTrue(scale.isSuccess());
//  }
//
//  @Test
//  public void stripMetadataAndAddInterlanceTest1() throws Exception {
//    String imagePath = classLoader.getResource("Images").getPath();
//    ImageResultDetail scale = service.stripMetadataAndAddInterlance(imagePath + "/TestImage.jpg", imagePath + "/TestImage.jpg",
//        new CustomGraphicsSettings(72, 75, new GraphicDimension(800, 800)), imagePath);
//    Assertions.assertTrue(scale.isSuccess());
//  }
}
