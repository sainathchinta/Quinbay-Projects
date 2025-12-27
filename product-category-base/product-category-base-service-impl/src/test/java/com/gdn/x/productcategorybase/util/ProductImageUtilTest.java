package com.gdn.x.productcategorybase.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ImagePathDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.service.FileStorageService;

public class ProductImageUtilTest {

  private static final String LOCATION_PATH_1 = "88/MTA-00011/LocationPath1.jpg";
  private static final String PATH_3 = "MTA-10110/LocationPath1.jpg";
  private static final String LOCATION_PATH_2 = "LocationPath2";
  private static String IMAGE_FINAL_DIRECTORY = "images1";

  private Product product;
  private File resourceDirectory;
  
  private ProductImageUtil util = new ProductImageUtil();

  @Mock
  private FileStorageService fileStorageService;
  
  @BeforeEach
  public void setup() throws IOException {
    MockitoAnnotations.initMocks(this);
    resourceDirectory = new File(IMAGE_FINAL_DIRECTORY);
    File finalImage = new File(resourceDirectory +File.separator + LOCATION_PATH_1);
    FileUtils.touch(finalImage);

    ProductImage productImage1 = new ProductImage();
    productImage1.setActive(false);
    productImage1.setLocationPath(LOCATION_PATH_1);

    ProductImage productImage2 = new ProductImage();
    productImage2.setActive(true);
    productImage2.setLocationPath(LOCATION_PATH_1);

    ProductImage productImage3 = new ProductImage();
    productImage3.setActive(false);
    productImage3.setLocationPath("");

    ProductImage productImage4 = new ProductImage();
    productImage4.setMarkForDelete(true);
    productImage4.setLocationPath("");

    ProductItemImage productItemImage1 = new ProductItemImage();
    productItemImage1.setActive(false);
    productItemImage1.setLocationPath(LOCATION_PATH_1);

    ProductItemImage productItemImage2 = new ProductItemImage();
    productItemImage2.setActive(true);
    productItemImage2.setLocationPath(LOCATION_PATH_1);

    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage3.setActive(false);
    productItemImage3.setLocationPath(LOCATION_PATH_2);

    ProductItemImage productItemImage4 = new ProductItemImage();
    productItemImage4.setMarkForDelete(true);
    productItemImage4.setLocationPath(LOCATION_PATH_2);

    ProductItem productItem = new ProductItem();
    productItem.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3, productItemImage4));


    product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2, productImage3, productImage4));
    product.setProductItems(Arrays.asList(productItem));
    ProductImageUtil.setFileStorageService(fileStorageService);
  }

  @AfterEach
  public void tearDown() throws IOException {
    FileUtils.deleteDirectory(resourceDirectory);
  }
  
  @Test
  public void generateImagePathTest() {
    ImagePathDTO imagePath = util.generateImagePath("MTA-0001", "MAC", "product-name", 1, true);
    Assertions.assertNotNull(imagePath);
  }
  
  @Test
  public void generateImagePathGreatedThan9Test() {
    ImagePathDTO imagePath = util.generateImagePath("MTA-0001", "MAC", "product-name", 10, true);
    Assertions.assertNotNull(imagePath);
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesTest() {
    Product product = getProduct();

    ProductImageUtil.setCommonImageFlagForProductAndItemImages(product, true);

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesComputecommonImageFalseTest() {
    Product product = getProduct();

    ProductImageUtil.setCommonImageFlagForProductAndItemImages(product, false);

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesSingleVariantTest() {
    Product product = getProduct();
    product.setProductItems(new ArrayList<>(product.getProductItems()).subList(0, 1));

    ProductImageUtil.setCommonImageFlagForProductAndItemImages(product, true);

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesNullItemImageTest() {
    ProductImage productImage1 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    ProductImage productImage3 = new ProductImage();
    ProductImage productImage4 = new ProductImage();
    ProductImage productImage5 = new ProductImage();
    ProductImage productImage6 = new ProductImage();
    ProductImage productImage7 = new ProductImage();
    ProductImage productImage8 = new ProductImage();
    ProductImage productImage9 = new ProductImage();
    productImage1.setLocationPath("location1");
    productImage2.setLocationPath("location2");
    productImage3.setLocationPath("location3");
    productImage4.setLocationPath("location4");
    productImage5.setLocationPath("location5");
    productImage6.setLocationPath("location6");
    productImage7.setLocationPath("location7");
    productImage8.setLocationPath("location8");
    productImage9.setLocationPath("location9");

    ProductItemImage productItemImage1 = new ProductItemImage();
    ProductItemImage productItemImage2 = new ProductItemImage();
    ProductItemImage productItemImage3 = new ProductItemImage();
    ProductItemImage productItemImage4 = new ProductItemImage();
    ProductItemImage productItemImage5 = new ProductItemImage();
    ProductItemImage productItemImage6 = new ProductItemImage();
    ProductItemImage productItemImage7 = new ProductItemImage();
    ProductItemImage productItemImage8 = new ProductItemImage();
    ProductItemImage productItemImage9 = new ProductItemImage();
    productItemImage1.setLocationPath("location1");
    productItemImage2.setLocationPath("location2");
    productItemImage3.setLocationPath("location3");
    productItemImage4.setLocationPath("location4");
    productItemImage5.setLocationPath("location5");
    productItemImage6.setLocationPath("location6");
    productItemImage7.setLocationPath("location7");
    productItemImage8.setLocationPath("location8");
    productItemImage9.setLocationPath("location9");

    Product product = getProduct();
    product.setProductImages(
        Arrays.asList(productImage1, productImage2, productImage3, productImage4, productImage5, productImage6,
            productImage7, productImage8, productImage9));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(
        Arrays.asList(productItemImage1, productItemImage2, productItemImage3, productItemImage4, productItemImage5,
            productItemImage6, productItemImage7, productItemImage8, productItemImage9)));

    ProductImageUtil.setCommonImageFlagForProductAndItemImages(product, true);
  }

  private Product getProduct() {
    ProductImage productImage1 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    ProductImage productImage3 = new ProductImage();
    productImage1.setLocationPath(LOCATION_PATH_1);
    productImage2.setLocationPath(LOCATION_PATH_2);
    productImage3.setLocationPath(LOCATION_PATH_1);
    productImage3.setMarkForDelete(true);

    ProductItemImage productItemImage1 = new ProductItemImage();
    ProductItemImage productItemImage2 = new ProductItemImage();
    ProductItemImage productItemImage3 = new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    productItemImage2.setLocationPath(LOCATION_PATH_2);
    productItemImage3.setLocationPath(LOCATION_PATH_1);
    productItemImage3.setMarkForDelete(true);

    ProductItem productItem1 = new ProductItem();
    ProductItem productItem2 = new ProductItem();
    productItem1.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));
    productItem2.setProductItemImages(Arrays.asList(productItemImage1, productItemImage3));

    Product product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    product.setProductItems(Arrays.asList(productItem1, productItem2));

    return product;
  }

  @Test
  public void getLocationPathAndCommonImagesTest() {
    Product product = getProduct();
    ProductItemImageUpdateRequest productItemImageUpdateRequest = new ProductItemImageUpdateRequest();
    List<ProductItemImageRequest> productItemImageRequestList = new ArrayList<>();
    ProductItemImageRequest productItemImageRequest = new ProductItemImageRequest();
    List<Image> itemImages = new ArrayList<>();
    Image image1 = new Image();
    image1.setLocationPath(LOCATION_PATH_1);
    Image image2 = new Image();
    image2.setLocationPath(LOCATION_PATH_2);
    productItemImageRequest.setItemImages(itemImages);
    productItemImageRequestList.add(productItemImageRequest);
    productItemImageUpdateRequest.setNewProductItemImages(productItemImageRequestList);

    ProductImageUtil.getLocationPathAndCommonImages(product, productItemImageUpdateRequest);
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesMoreThan8CommonImagesTest() {
    Product product = getProduct();
    product.getProductImages().get(0).setCommonImage(false);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, true,
        productPublishUpdateDTO, new HashSet<>());

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesMoreThan8CommonImagesFalseTest() {
    Product product = getProduct();
    product.getProductImages().get(0).setCommonImage(false);
    product.getProductImages().get(1).setCommonImage(true);
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, true,
        productPublishUpdateDTO, new HashSet<>());

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesWithProductPublishDtoTest() {
    Product product = getProduct();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, true, productPublishUpdateDTO, new HashSet<>());

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesWithProductPublishDtoComputecommonImageFalseTest() {
    Product product = getProduct();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, false, productPublishUpdateDTO, new HashSet<>());

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesWithProductPublishDtoSingleVariantTest() {
    Product product = getProduct();
    product.setProductItems(new ArrayList<>(product.getProductItems()).subList(0, 1));
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, true, productPublishUpdateDTO, new HashSet<>());

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesWithProductPublishDtoNullItemImageTest() {
    ProductImage productImage1 = new ProductImage();
    ProductImage productImage2 = new ProductImage();
    ProductImage productImage3 = new ProductImage();
    ProductImage productImage4 = new ProductImage();
    ProductImage productImage5 = new ProductImage();
    ProductImage productImage6 = new ProductImage();
    ProductImage productImage7 = new ProductImage();
    ProductImage productImage8 = new ProductImage();
    ProductImage productImage9 = new ProductImage();
    productImage1.setLocationPath("location1");
    productImage2.setLocationPath("location2");
    productImage3.setLocationPath("location3");
    productImage4.setLocationPath("location4");
    productImage5.setLocationPath("location5");
    productImage6.setLocationPath("location6");
    productImage7.setLocationPath("location7");
    productImage8.setLocationPath("location8");
    productImage9.setLocationPath("location9");

    ProductItemImage productItemImage1 = new ProductItemImage();
    ProductItemImage productItemImage2 = new ProductItemImage();
    ProductItemImage productItemImage3 = new ProductItemImage();
    ProductItemImage productItemImage4 = new ProductItemImage();
    ProductItemImage productItemImage5 = new ProductItemImage();
    ProductItemImage productItemImage6 = new ProductItemImage();
    ProductItemImage productItemImage7 = new ProductItemImage();
    ProductItemImage productItemImage8 = new ProductItemImage();
    ProductItemImage productItemImage9 = new ProductItemImage();
    productItemImage1.setLocationPath("location1");
    productItemImage2.setLocationPath("location2");
    productItemImage3.setLocationPath("location3");
    productItemImage4.setLocationPath("location4");
    productItemImage5.setLocationPath("location5");
    productItemImage6.setLocationPath("location6");
    productItemImage7.setLocationPath("location7");
    productItemImage8.setLocationPath("location8");
    productItemImage9.setLocationPath("location9");


    Product product = getProduct();
    product.setProductImages(
        Arrays.asList(productImage1, productImage2, productImage3, productImage4, productImage5, productImage6,
            productImage7, productImage8, productImage9));
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(
        Arrays.asList(productItemImage1, productItemImage2, productItemImage3, productItemImage4, productItemImage5,
            productItemImage6, productItemImage7, productItemImage8, productItemImage9)));
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, true, productPublishUpdateDTO, new HashSet<>());

  }

  @Test
  public void setCommonImageFlagForProductAndItemImagesWithProductPublishDtoMoreThan8CommonImagesTest() {
    Product product = getProduct();
    ProductPublishUpdateDTO productPublishUpdateDTO = new ProductPublishUpdateDTO(null, false, new HashSet<>());

    ProductImageUtil.setCommonImageFlagForProductAndItemImagesWithProductPublishDto(product, true, productPublishUpdateDTO, new HashSet<>());

    Assertions.assertFalse(product.getProductImages().get(1).isCommonImage());
    Assertions.assertFalse(product.getProductItems().get(0).getProductItemImages().get(1).isCommonImage());
  }

  @Test
  public void setActiveFlagInProductAndItemImagesTest() {
    ProductImageUtil.setActiveFlagInProductAndItemImages(product);
    Assertions.assertTrue(product.getProductImages().get(0).isActive());
  }

  @Test
  public void setActiveFlagInProductAndItemImagesEmptyImagesTest() {
    product.setProductImages(new ArrayList<>());
    product.getProductItems().get(0).setProductItemImages(new ArrayList<>());
    ProductImageUtil.setActiveFlagInProductAndItemImages(product);
  }

  @Test
  public void setActiveFlagInProductAndItemImagesEmptyItemsTest() {
    product.setProductItems(new ArrayList<>());
    ProductImageUtil.setActiveFlagInProductAndItemImages(product);
  }

  @Test
  public void setProductMainImageFromCommonImagesEmptyRequestTest() {
    ProductImageUtil.setProductMainImageFromCommonImagesRequest(getProduct(), new ArrayList<>(), false);
  }

  @Test
  public void setProductMainImageFromCommonImagesTrueRequestTest() {
    ProductImageUtil.setProductMainImageFromCommonImagesRequest(getProduct(), new ArrayList<>(), true);
  }

  @Test
  public void setProductMainImageFromCommonImagesTrueRequestCommonImageTest() {
    Product product = getProduct();
    product.getProductImages().get(0).setCommonImage(true);
    ProductImageUtil.setProductMainImageFromCommonImagesRequest(product, new ArrayList<>(), true);
  }

  @Test
  public void setProductMainImageFromCommonImagesTrue2RequestCommonImageTest() {
    Product product = getProduct();
    product.getProductImages().get(0).setCommonImage(true);
    product.getProductImages().get(1).setCommonImage(true);
    ProductImageUtil.setProductMainImageFromCommonImagesRequest(product, new ArrayList<>(), true);
  }

  @Test
  public void setProductMainImageFromCommonImagesNotMainImageRequestTest() {
    Image image = new Image();
    ProductImageUtil.setProductMainImageFromCommonImagesRequest(getProduct(), Collections.singletonList(image), false);
  }

  @Test
  public void setProductMainImageFromCommonImagesRequestTest() {
    Image image = new Image();
    image.setMainImages(true);
    image.setLocationPath(LOCATION_PATH_1);
    Product product = getProduct();
    product.getProductImages().get(0).setMainImages(false);
    product.getProductImages().get(0).setLocationPath(PATH_3);
    ProductImageUtil.setProductMainImageFromCommonImagesRequest(product, Collections.singletonList(image), false);
    Assertions.assertTrue(product.getProductImages().get(0).isMainImages());

  }

  @Test
  public void checkCommonImageMaxCountNewProductTest() {
    product.getProductImages().get(0).setCommonImage(true);
    product.getProductImages().get(0).setMarkForDelete(false);
    product.getProductImages().get(0).setOriginalImage(true);

    product.getProductImages().get(1).setCommonImage(true);
    product.getProductImages().get(1).setMarkForDelete(true);

    product.getProductImages().get(2).setCommonImage(true);
    product.getProductImages().get(2).setMarkForDelete(false);
    product.getProductImages().get(2).setOriginalImage(false);

    ProductImageUtil.checkCommonImageMaxCount(true, false, false, false, false, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxCountActivatedProductTest() {
    product.getProductImages().get(0).setCommonImage(true);
    product.getProductImages().get(0).setMarkForDelete(false);
    product.getProductImages().get(0).setActive(true);

    product.getProductImages().get(1).setCommonImage(true);
    product.getProductImages().get(1).setActive(false);

    ProductImageUtil.checkCommonImageMaxCount(false, true, false, false, false, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxCountEditedProductTest() {
    product.getProductImages().get(0).setCommonImage(true);
    product.getProductImages().get(0).setMarkForDelete(false);
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setEdited(true);

    product.getProductImages().get(1).setCommonImage(true);
    product.getProductImages().get(1).setMarkForDelete(false);
    product.getProductImages().get(1).setActive(true);
    product.getProductImages().get(1).setEdited(false);

    product.getProductImages().get(2).setCommonImage(true);
    product.getProductImages().get(2).setMarkForDelete(false);
    product.getProductImages().get(2).setActive(false);
    product.getProductImages().get(2).setEdited(true);

    product.getProductImages().get(3).setCommonImage(true);
    product.getProductImages().get(3).setMarkForDelete(false);
    product.getProductImages().get(3).setActive(false);
    product.getProductImages().get(3).setEdited(false);

    ProductImageUtil.checkCommonImageMaxCount(false, false, true, false, false, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxCountRevisedProductTest() {
    product.getProductImages().get(0).setCommonImage(true);
    product.getProductImages().get(0).setMarkForDelete(false);
    product.getProductImages().get(0).setActive(true);
    product.getProductImages().get(0).setEdited(true);
    product.getProductImages().get(0).setRevised(true);
    product.getProductImages().get(0).setOriginalImage(true);


    product.getProductImages().get(1).setCommonImage(true);
    product.getProductImages().get(1).setMarkForDelete(false);
    product.getProductImages().get(1).setActive(true);
    product.getProductImages().get(1).setEdited(false);

    product.getProductImages().get(2).setCommonImage(true);
    product.getProductImages().get(2).setMarkForDelete(false);
    product.getProductImages().get(2).setActive(false);
    product.getProductImages().get(2).setEdited(true);
    product.getProductImages().get(2).setOriginalImage(true);
    product.getProductImages().get(2).setRevised(false);

    product.getProductImages().get(3).setCommonImage(true);
    product.getProductImages().get(3).setMarkForDelete(false);
    product.getProductImages().get(3).setActive(false);
    product.getProductImages().get(3).setEdited(false);
    product.getProductImages().get(3).setOriginalImage(true);
    product.getProductImages().get(3).setRevised(true);

    ProductImageUtil.checkCommonImageMaxCount(false, false, false, true, false, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxCountRevisedUnActivatedProductTest() {
    product.getProductImages().get(0).setCommonImage(true);
    product.getProductImages().get(0).setMarkForDelete(false);
    product.getProductImages().get(0).setRevised(true);
    product.getProductImages().get(0).setOriginalImage(true);

    product.getProductImages().get(1).setCommonImage(true);
    product.getProductImages().get(1).setMarkForDelete(false);
    product.getProductImages().get(1).setRevised(true);
    product.getProductImages().get(1).setOriginalImage(false);

    product.getProductImages().get(2).setCommonImage(true);
    product.getProductImages().get(2).setMarkForDelete(false);
    product.getProductImages().get(2).setRevised(false);
    product.getProductImages().get(2).setOriginalImage(true);

    product.getProductImages().get(3).setCommonImage(true);
    product.getProductImages().get(3).setMarkForDelete(false);
    product.getProductImages().get(3).setRevised(false);
    product.getProductImages().get(3).setOriginalImage(false);

    ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxCountTest() {

    ProductImage productImage = new ProductImage();
    productImage.setCommonImage(true);
    productImage.setMarkForDelete(false);
    productImage.setOriginalImage(true);
    productImage.setLocationPath(LOCATION_PATH_2);
    product.setProductImages(Arrays
      .asList(productImage, productImage, productImage, productImage, productImage, productImage,
        productImage, productImage, productImage));

    ProductImageUtil.checkCommonImageMaxCount(true, false, false, false, false, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxRevisedCountTest() {

    ProductImage productImage = new ProductImage();
    productImage.setCommonImage(true);
    productImage.setMarkForDelete(false);
    productImage.setOriginalImage(true);
    productImage.setLocationPath(LOCATION_PATH_2);
    product.setProductImages(Arrays
      .asList(productImage, productImage, productImage, productImage, productImage, productImage,
        productImage, productImage, productImage));

    ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, new HashSet<>());
  }

  @Test
  public void getImageFileNameTest() {
    String LOCATION_PATH_1 = "/file/location/sample.jpg";
    String path = ProductImageUtil.getImageFileName(LOCATION_PATH_1);
    Assertions.assertNotNull(path);
  }

  @Test
  public void checkCommonImageMaxRevisedCountMaxTest1() {
    ProductImage productImage = new ProductImage();
    productImage.setCommonImage(true);
    productImage.setMarkForDelete(false);
    productImage.setOriginalImage(true);
    productImage.setLocationPath(LOCATION_PATH_2);
    product.setProductImages(Arrays
        .asList(productImage, productImage, productImage, productImage));

    ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxRevisedCountTest1() {

    List<ProductImage> productImageList = new ArrayList<>();
    List<ProductItemImage> productItemImageList = new ArrayList<>();

    for (int i = 1; i <= 9; i++ ) {
      ProductImage productImage = new ProductImage();
      productImage.setLocationPath(String.valueOf(i));
      productImage.setCommonImage(true);

      ProductItemImage productItemImage = new ProductItemImage();
      productItemImage.setLocationPath(String.valueOf(i));
      productItemImage.setCommonImage(true);

      productImageList.add(productImage);
      productItemImageList.add(productItemImage);
    }

    ProductImage productImage = new ProductImage();
    productImage.setLocationPath("10");
    productImage.setMarkForDelete(true);
    productImageList.add(productImage);

    product.setProductImages(productImageList);
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(productItemImageList));

    ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxRevisedCountNoItemImageTest1() {

    List<ProductImage> productImageList = new ArrayList<>();
    List<ProductItemImage> productItemImageList = new ArrayList<>();

    for (int i = 1; i <= 9; i++ ) {
      ProductImage productImage = new ProductImage();
      productImage.setLocationPath(String.valueOf(i));
      productImage.setCommonImage(true);

      ProductItemImage productItemImage = new ProductItemImage();
      productItemImage.setLocationPath(String.valueOf(i));
      productItemImage.setCommonImage(true);

      productImageList.add(productImage);
      productItemImageList.add(productItemImage);
    }

    ProductImage productImage = new ProductImage();
    productImage.setLocationPath("10");
    productImage.setCommonImage(true);
    productImageList.add(productImage);

    product.setProductImages(productImageList);
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(productItemImageList));

    ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, new HashSet<>());
  }

  @Test
  public void checkCommonImageMaxRevisedCountNoItemImageCommonImageFalseTest1() {

    List<ProductImage> productImageList = new ArrayList<>();
    List<ProductItemImage> productItemImageList = new ArrayList<>();

    for (int i = 1; i <= 9; i++ ) {
      ProductImage productImage = new ProductImage();
      productImage.setLocationPath(String.valueOf(i));
      productImage.setCommonImage(true);

      ProductItemImage productItemImage = new ProductItemImage();
      productItemImage.setLocationPath(String.valueOf(i));
      productItemImage.setCommonImage(false);

      productImageList.add(productImage);
      productItemImageList.add(productItemImage);
    }

    ProductImage productImage = new ProductImage();
    productImage.setLocationPath("10");
    productImage.setCommonImage(true);
    productImageList.add(productImage);

    product.setProductImages(productImageList);
    product.getProductItems().forEach(productItem -> productItem.setProductItemImages(productItemImageList));

    ProductImageUtil.checkCommonImageMaxCount(false, false, false, false, true, product, new HashSet<>());
  }


  @Test
  public void resetProductImagesTest() {
    Product product = getProductForImageReset();
    ProductImageUtil.resetProductImages(product, true);
    Assertions.assertTrue(product.getProductImages().get(0).isMainImages());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
  }

  @Test
  public void resetProductImagesNoVariantMainImageTest() {
    Product product = getProductForImageReset();
    product.getProductItems().get(0).getProductItemImages()
        .forEach(productItemImage -> productItemImage.setMainImages(false));
    ProductImageUtil.resetProductImages(product, true);
    Assertions.assertTrue(product.getProductImages().get(0).isMainImages());
    Assertions.assertTrue(product.getProductImages().get(1).isMarkForDelete());
  }

  @Test
  public void resetProductImagesEmptyProductImagesTest() {
    Product product = getProductForImageReset();
    product.setProductImages(new ArrayList<>());
    ProductImageUtil.resetProductImages(product, true);
  }

  @Test
  public void resetProductImagesProductMainImagesTest() {
    Product product = getProductForImageReset();
    product.getProductImages().get(0).setMainImages(true);
    ProductImageUtil.resetProductImages(product, true);
  }

  @Test
  public void resetProductImagesProductMainImagesOffTest() {
    Product product = getProductForImageReset();
    product.getProductImages().get(0).setMainImages(true);
    ProductImageUtil.resetProductImages(product, false);
  }

  private List<ProductItem> getProductItems() {
    ProductItemImage productItemImage1 =new ProductItemImage();
    productItemImage1.setLocationPath(LOCATION_PATH_1);
    productItemImage1.setMainImages(true);

    ProductItemImage productItemImage2 =new ProductItemImage();
    productItemImage2.setLocationPath(LOCATION_PATH_1);

    ProductItemImage productItemImage3 =new ProductItemImage();
    productItemImage3.setMarkForDelete(true);

    ProductItemImage productItemImage4 =new ProductItemImage();
    productItemImage4.setLocationPath(PATH_3);


    ProductItem productItem1 = new ProductItem();
    productItem1.setProductItemImages(Arrays.asList(productItemImage1, productItemImage2, productItemImage3));

    ProductItem productItem2 = new ProductItem();
    productItem2.setMarkForDelete(true);
    productItem2.setProductItemImages(Arrays.asList(productItemImage4));

    ProductItem productItem3 = new ProductItem();
    productItem3.setProductItemImages(new ArrayList<>());

    return Arrays.asList(productItem1, productItem2, productItem3);
  }

  private Product getProductForImageReset() {
    ProductImage productImage1 = new ProductImage();
    productImage1.setLocationPath(LOCATION_PATH_1);

    ProductImage productImage2 = new ProductImage();
    productImage2.setLocationPath(PATH_3);

    ProductImage productImage3 = new ProductImage();
    productImage3.setMarkForDelete(true);

    Product product = new Product();
    product.setProductImages(Arrays.asList(productImage1, productImage2, productImage3));
    product.setProductItems(getProductItems());

    return product;
  }
}
