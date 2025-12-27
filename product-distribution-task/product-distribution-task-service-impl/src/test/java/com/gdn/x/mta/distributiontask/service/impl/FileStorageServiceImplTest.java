package com.gdn.x.mta.distributiontask.service.impl;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.service.api.GcsService;
import com.gdn.x.mta.distributiontask.service.impl.config.GcsProperties;


public class FileStorageServiceImplTest {

  private static final String SOURCE_DIRECTORY = "imageSourceDirectory";
  private static final String LOCATION_PATH = "/nike_test_product_and_item_images_full01_hhp7330d.jpg";
  private static final String CATALOG_NAME = "/catalog-image";
  private static final String FILE = "/sample.txt";
  private static final String FILE_PATH = "src/test/resources/filestr/sample.txt";
  private static final String BUCKET_NAME = "bucket-name";
  private static final String CATALOG_IMAGE = "catalog-image";
  private static final String SOURCE_IMAGE = "/source-image";
  private static final String IMAGE_SOURCE_DIRECTORY = "source/";
  private static final String IMAGE_FINAL_DIRECTORY = "final/";
  private static final String IMAGE_1 = "1.png";
  private static final String IMAGE_2 = "2.png";
  private static File file;
  private File resourceDirectory;
  private File resourceDirectoryFinal;

  @InjectMocks
  private FileStorageServiceImpl fileStorageServiceImpl;
  @Mock
  private GcsService gcsSourceImageService;

  @Mock
  private GcsProperties gcsProperties;

  @BeforeEach
  public void setUp() throws IOException {
    MockitoAnnotations.openMocks(this);
    ReflectionTestUtils.setField(fileStorageServiceImpl, "imageSourceDirectory","src/test/resources/filestr");
    createFile();
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn("bucket-name");
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn("image-directory");
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn("full-image-directory");
  }

  @AfterEach
  public void tearDown() throws IOException {
    FileUtils.deleteDirectory(resourceDirectory);
    FileUtils.deleteDirectory(resourceDirectoryFinal);
  }

  @Test
   void testDeleteOriginalImages() {
    File image = Mockito.mock(File.class);
    Mockito.when(image.exists()).thenReturn(true);
    fileStorageServiceImpl.deleteOriginalImages(LOCATION_PATH);
  }

  @Test
   void testFalseDeleteOriginalImages() {
    String locationPath = SOURCE_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(image.exists()).thenReturn(true);
    fileStorageServiceImpl.deleteOriginalImages(locationPath);
  }

  @Test
   void testDelete2OriginalImages() throws IOException {
    mockImageFile(FILE_PATH);
    fileStorageServiceImpl.deleteOriginalImages(FILE);
  }

  private void mockImageFile(String filePath) throws IOException {
    mockFile(filePath);
  }

  private void mockFile(String filePath) throws IOException {
    file = new File(filePath);
    file.mkdirs();
  }

  @Test
   void deleteFromGcsTest() {
    String locationPath = SOURCE_DIRECTORY + LOCATION_PATH;
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn("source/");
    Mockito.when(gcsSourceImageService.deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(true);
    fileStorageServiceImpl.deleteFromGcs(locationPath);
    Mockito.verify(gcsSourceImageService).deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getSourceImageDirectory();
  }

  @Test
   void deleteFileTest() {
    String locationPath = SOURCE_DIRECTORY + LOCATION_PATH;
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn("source/");
    Mockito.when(gcsSourceImageService.deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(false);
    fileStorageServiceImpl.deleteFromGcs(locationPath);
    Mockito.verify(gcsSourceImageService,Mockito.times(1)).deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"));;
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getSourceImageDirectory();
  }

  @Test
   void validateProductImagesTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceImpl, "imageSourceDirectory",IMAGE_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(CATALOG_IMAGE);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE);
    Mockito.when(fileStorageServiceImpl.isFinalImageFileExist(IMAGE_FINAL_DIRECTORY)).thenReturn(false);
    Mockito.when(gcsSourceImageService.isFileExists(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(true);
    Product product = new Product();
    product.setProductImages(getProductImages());
    ProductItem productItem = new ProductItem();
    productItem.setProductItemImages(getProductItemImages());
    product.setProductItems(List.of(productItem));

    fileStorageServiceImpl.validateProductImages(product);

    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsSourceImageService)
        .isFileExists(BUCKET_NAME, SOURCE_IMAGE + "/" + "catalog-image" + "/" + IMAGE_1);
    Assertions.assertTrue(
        gcsSourceImageService.isFileExists(Mockito.anyString(),Mockito.anyString()));
  }

  @Test
   void validateProductImagesSourceImageNotFoundTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceImpl, "imageSourceDirectory", IMAGE_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(CATALOG_IMAGE);
    Product product = new Product();
    product.setProductImages(getProductInvalidSourceImages());
    try {
      Assertions.assertThrows(Exception.class,
        () -> fileStorageServiceImpl.validateProductImages(product));
    } finally {
      Mockito.verify(gcsProperties).getPathPrefix();
    }
  }

  @Test
   void validateProductImagesFinalImageNotFoundTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceImpl, "imageSourceDirectory", IMAGE_SOURCE_DIRECTORY);
    Product product = new Product();
    product.setProductImages(getProductInvalidFinalImages());
    Assertions.assertThrows(Exception.class,
      () -> fileStorageServiceImpl.validateProductImages(product));
  }

  @Test
   void updateNewlyAddedImagePathGcsEnableTest() {
    ProductImage  productImage1 = new ProductImage();
    productImage1.setLocationPath(IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(LOCATION_PATH);
    productImage3.setActive(true);

    String id = UUID.randomUUID().toString();
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(IMAGE_1);
    productItemImage1.setId(id);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(IMAGE_2);
    productItemImage2.setId(id);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(LOCATION_PATH);
    productItemImage3.setActive(true);
    productItemImage3.setId(id);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProductItem.setId(id);
    newProductItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));
    newProduct.setProductItems(List.of(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(List.of(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProductItem.setId(id);
    existingProductItem.setProductItemImages(List.of(productItemImage1));
    existingProduct.setProductItems(List.of(existingProductItem));

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(CATALOG_IMAGE);

    fileStorageServiceImpl.updateNewlyAddedImagePath(newProduct, existingProduct);

    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(4)).getPathPrefix();

    Assertions.assertTrue(
        newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
    Assertions.assertTrue(
        newProduct.getProductItems().get(0).getProductItemImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
  }

  @Test
   void updateNewlyAddedImagePathGcsDisableTest() {
    ProductImage  productImage1 = new ProductImage();
    productImage1.setLocationPath(IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(LOCATION_PATH);
    productImage3.setActive(true);

    String id = UUID.randomUUID().toString();
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(IMAGE_1);
    productItemImage1.setId(id);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(IMAGE_2);
    productItemImage2.setId(id);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(LOCATION_PATH);
    productItemImage3.setActive(true);
    productItemImage3.setId(id);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProductItem.setId(id);
    newProductItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));
    newProduct.setProductItems(List.of(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(List.of(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProductItem.setId(id);
    existingProductItem.setProductItemImages(List.of(productItemImage1));
    existingProduct.setProductItems(List.of(existingProductItem));

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);

    fileStorageServiceImpl.updateNewlyAddedImagePath(newProduct, existingProduct);

    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();

    Assertions.assertFalse(
        newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
    Assertions.assertFalse(
        newProduct.getProductItems().get(0).getProductItemImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
  }

  @Test
   void updateNewlyAddedImagePathGcsEnableEmptyProductItemImagesTest() {
    ProductImage  productImage1 = new ProductImage();
    productImage1.setLocationPath(IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(LOCATION_PATH);
    productImage3.setActive(true);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProduct.setProductItems(List.of(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(List.of(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProduct.setProductItems(List.of(existingProductItem));

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);

    fileStorageServiceImpl.updateNewlyAddedImagePath(newProduct, existingProduct);

    Mockito.verify(gcsProperties).isSourceImageEnabled();

    Assertions.assertFalse(
        newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
  }

  @Test
   void updateNewlyAddedImagePathGcsEnableImagePrefixAddedTest() {
    ProductImage  productImage1 = new ProductImage();
    productImage1.setLocationPath(CATALOG_IMAGE + IMAGE_1);
    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(CATALOG_IMAGE + IMAGE_2);
    ProductImage productImage3 = new ProductImage();
    productImage3.setLocationPath(CATALOG_IMAGE + LOCATION_PATH);
    productImage3.setActive(true);

    String id = UUID.randomUUID().toString();
    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setLocationPath(CATALOG_IMAGE + IMAGE_1);
    productItemImage1.setId(id);
    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setLocationPath(CATALOG_IMAGE + IMAGE_2);
    productItemImage2.setId(id);
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setLocationPath(CATALOG_IMAGE + LOCATION_PATH);
    productItemImage3.setActive(true);
    productItemImage3.setId(id);

    Product newProduct = new Product();
    newProduct.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    ProductItem newProductItem = new ProductItem();
    newProductItem.setId(id);
    newProductItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));
    newProduct.setProductItems(List.of(newProductItem));

    Product existingProduct = new Product();
    existingProduct.setProductImages(List.of(productImage1));
    ProductItem existingProductItem = new ProductItem();
    existingProductItem.setId(id);
    existingProductItem.setProductItemImages(List.of(productItemImage1));
    existingProduct.setProductItems(List.of(existingProductItem));

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(CATALOG_IMAGE);

    fileStorageServiceImpl.updateNewlyAddedImagePath(newProduct, existingProduct);

    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();

    Assertions.assertTrue(
        newProduct.getProductImages().get(1).getLocationPath().contains(CATALOG_IMAGE));
    Assertions.assertTrue(
        newProduct.getProductItems().get(0).getProductItemImages().get(1).getLocationPath().contains(CATALOG_IMAGE));

  }

  @Test
   void updateNewlyAddedImagePathNoImageAndItemsTest() {
    Product newProduct = new Product();
    Product existingProduct = new Product();

    fileStorageServiceImpl.updateNewlyAddedImagePath(newProduct, existingProduct);
  }


  private List<ProductImage> getProductImages() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setMarkForDelete(true);

    ProductImage productImage2 = new ProductImage();
    productImage2.setActive(false);
    productImage2.setLocationPath(IMAGE_1);

    ProductImage productImage3 = new ProductImage();
    productImage3.setActive(false);
    productImage3.setLocationPath("catalog-image" + "/" + IMAGE_1);

    ProductImage productImage4 = new ProductImage();
    productImage4.setActive(true);
    productImage4.setLocationPath(IMAGE_1);

    return Arrays.asList(productImage1, productImage2, productImage3, productImage4);
  }

  private List<ProductItemImage> getProductItemImages() {
    ProductItemImage productImage1 = new ProductItemImage();
    productImage1.setMarkForDelete(true);

    ProductItemImage productImage2 = new ProductItemImage();
    productImage2.setActive(false);
    productImage2.setLocationPath(IMAGE_1);

    ProductItemImage productImage3 = new ProductItemImage();
    productImage3.setActive(false);
    productImage3.setLocationPath("catalog-image" + "/" + IMAGE_1);

    ProductItemImage productImage4 = new ProductItemImage();
    productImage4.setActive(true);
    productImage4.setLocationPath(IMAGE_1);

    return Arrays.asList(productImage1, productImage2, productImage3, productImage4);
  }

  private List<ProductImage> getProductInvalidSourceImages() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setActive(false);
    productImage1.setLocationPath(IMAGE_2);

    return List.of(productImage1);
  }

  private List<ProductImage> getProductInvalidFinalImages() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setActive(true);
    productImage1.setLocationPath(IMAGE_2);

    return List.of(productImage1);
  }

  private void createFile() throws IOException {
    resourceDirectory = new File(IMAGE_SOURCE_DIRECTORY);
    resourceDirectoryFinal = new File(IMAGE_FINAL_DIRECTORY);
    File imageFile = new File(resourceDirectory + "/" + IMAGE_1);
    File imageFileFinal = new File(resourceDirectoryFinal + "/" + IMAGE_1);
    FileUtils.touch(imageFile);
    FileUtils.touch(imageFileFinal);
  }

  @Test
   void isFinalImageFileExistTest() {
    Mockito.when(gcsSourceImageService.isFileExists("bucket-name", "image-directory/full-image-directory/file.jpg")).thenReturn(false);
    boolean result = fileStorageServiceImpl.isFinalImageFileExist("file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
   void isFinalImageFileExistTest2() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsSourceImageService.isFileExists("bucket-name", "image-directory/full-image-directory/file.jpg")).thenReturn(false);
    boolean result = fileStorageServiceImpl.isFinalImageFileExist("prefix/file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
   void isFinalImageFileExistTest3() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    boolean result = fileStorageServiceImpl.isFinalImageFileExist("file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
   void isFinalImageFileExistTest4() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsSourceImageService.isFileExists("bucket-name", "image-directory/full-image-directory/file.jpg"))
        .thenReturn(true);
    boolean result = fileStorageServiceImpl.isFinalImageFileExist("prefix/file.jpg");
    Assertions.assertTrue(result);
  }
  @Test
   void isFinalImageFileExistTest5() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    boolean result = fileStorageServiceImpl.isFinalImageFileExist("file.jpg");
    Assertions.assertTrue(result);
  }
  @Test
   void isFinalImageFileExistFalse() {
    ReflectionTestUtils.setField(fileStorageServiceImpl,"fullImageDirectory","src/test/resources/");
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsSourceImageService.isFileExists(Mockito.any(), Mockito.anyString())).thenReturn(false);
    boolean result = fileStorageServiceImpl.isFinalImageFileExist("resize/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Assertions.assertFalse(result);
  }

}

