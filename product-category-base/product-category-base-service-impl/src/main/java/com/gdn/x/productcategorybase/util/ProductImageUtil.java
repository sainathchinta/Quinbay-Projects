package com.gdn.x.productcategorybase.util;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.lang3.tuple.Pair;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ImagePathDTO;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.ProductItemImageRequest;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.service.FileStorageService;

public class ProductImageUtil {
  
  private static int MAX_COMMON_IMAGE = 8;

  private static FileStorageService fileStorageService;

  public static void setFileStorageService(FileStorageService fileStorageService) {
    ProductImageUtil.fileStorageService = fileStorageService;
  }

  public static ImagePathDTO generateImagePath(String productCode, String brand, String productName, int sequence, boolean mainImage){
    StringBuilder validImageFilename = new StringBuilder(brand.toLowerCase().replaceAll("[^a-zA-Z0-9]+", "-"));
    validImageFilename.append("_");
    validImageFilename.append(productName.toLowerCase().replaceAll("[^a-zA-Z0-9]+", "-"));
    validImageFilename.append("_full");
    validImageFilename.append(sequence <= 9 ? "0" + sequence : String.valueOf(sequence));
    validImageFilename.append(".jpg");
    
    return new ImagePathDTO("/" + productCode + "/" + validImageFilename, mainImage);
  }

  public static void setCommonImageFlagForProductAndItemImages(Product product, boolean computeCommonImage) {
    if (computeCommonImage && product.getProductItems().size() > 1) {
      int commonImageCount = 0;
      Map<String, List<ProductItemImage>> imagePathAndItemImageMap =
          product.getProductItems().stream().flatMap(productItem -> productItem.getProductItemImages().stream()).collect(Collectors.groupingBy(ProductItemImage::getLocationPath));
      for (ProductImage productImage : product.getProductImages()) {
        if (!productImage.isMarkForDelete()) {
          List<ProductItemImage> productItemImages = imagePathAndItemImageMap.get(productImage.getLocationPath());
          if (CollectionUtils.isNotEmpty(productItemImages)) {
            List<ProductItemImage> tempProductImages =
                productItemImages.stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
                    .collect(Collectors.toList());
            if (tempProductImages.size() == product.getProductItems().size()) {
              setCommonImageFlag(productImage, tempProductImages, true);
              commonImageCount++;
            } else {
              setCommonImageFlag(productImage, tempProductImages, false);
            }
          }
        }
      }
    }
  }

  public static void setCommonImageFlagForProductAndItemImagesWithProductPublishDto(Product product,
      boolean computeCommonImage, ProductPublishUpdateDTO productPublishUpdateDTO, Set<String> commonImageLocationPathSet) {
    if (computeCommonImage && product.getProductItems().size() > 1) {
      int commonImageCount = 0;
      Map<String, List<ProductItemImage>> imagePathAndItemImageMap =
          product.getProductItems().stream().flatMap(productItem -> productItem.getProductItemImages().stream())
              .collect(Collectors.groupingBy(ProductItemImage::getLocationPath));
      for (ProductImage productImage : product.getProductImages()) {
        if (!productImage.isMarkForDelete()) {
          List<ProductItemImage> productItemImages = imagePathAndItemImageMap.get(productImage.getLocationPath());
          if (CollectionUtils.isNotEmpty(productItemImages)) {
            List<ProductItemImage> tempProductImages =
                productItemImages.stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
                    .collect(Collectors.toList());
            if (tempProductImages.size() == product.getProductItems().size()) {
              setCommonImageFlagWithProductPublishDto(productImage, tempProductImages, true, productPublishUpdateDTO, commonImageLocationPathSet);
              commonImageCount++;
            } else {
              setCommonImageFlagWithProductPublishDto(productImage, tempProductImages, false, productPublishUpdateDTO, commonImageLocationPathSet);
            }
          }
        }
      }
    }
  }

  public static void setProductMainImageFromCommonImagesRequest(Product product, List<Image> commonImages,
      boolean setCommonImageAsMainImage) {
    if (CollectionUtils.isNotEmpty(commonImages)) {
      String locationPath = StringUtils.EMPTY;
      for (Image image : commonImages) {
        if (image.isMainImages()) {
          locationPath = image.getLocationPath().substring(image.getLocationPath().lastIndexOf(Constants.PATH_SEPARATOR) + 1);
        }
      }
      boolean commonImageExistsInProductImage = false;
      // Making sure image exists in product images before changing mainImage as false for existing image
      for (ProductImage productImage : product.getProductImages()) {
        if (productImage.getLocationPath().contains(locationPath)) {
          commonImageExistsInProductImage = true;
        }
      }
      if (commonImageExistsInProductImage) {
        for (ProductImage productImage : product.getProductImages()) {
          if (productImage.getLocationPath().contains(locationPath)) {
            productImage.setMainImages(true);
          } else {
            productImage.setMainImages(false);
          }
        }
      }
    } else {
      if (setCommonImageAsMainImage) {
        boolean productHasCommonImage = false;
        for (ProductImage productImage : product.getProductImages()) {
          if (productImage.isCommonImage()) {
            productHasCommonImage = true;
            break;
          }
        }
        boolean productMainImageIsSet = false;
        if (productHasCommonImage) {
          for (ProductImage productImage : product.getProductImages()) {
            if (productImage.isCommonImage() && !productMainImageIsSet) {
              productImage.setMainImages(true);
              productMainImageIsSet = true;
            } else {
              productImage.setMainImages(false);
            }
          }
        }
      }
    }
  }

  private static void setCommonImageFlag(ProductImage productImage, List<ProductItemImage> productItemImages,
      boolean commonImage) {
    productImage.setCommonImage(commonImage);
    if (CollectionUtils.isNotEmpty(productItemImages)) {
      productItemImages.forEach(productItemImage -> productItemImage.setCommonImage(commonImage));
    }
  }

  private static void setCommonImageFlagWithProductPublishDto(ProductImage productImage,
      List<ProductItemImage> productItemImages, boolean commonImage, ProductPublishUpdateDTO productPublishUpdateDTO,
      Set<String> commonImageLocationPathSet) {
    if (productImage.isCommonImage() != commonImage) {
      productImage.setCommonImage(commonImage);
      if (commonImage) {
        commonImageLocationPathSet.add(productImage.getLocationPath());
      } else {
        productPublishUpdateDTO.setProductLevelDataUpdated(true);
      }
    }
    if (CollectionUtils.isNotEmpty(productItemImages)) {
      productItemImages.forEach(productItemImage -> productItemImage.setCommonImage(commonImage));
    }
  }

  public static List<LocationPathAndCommonImage> getLocationPathAndCommonImages(Product product,
      ProductItemImageUpdateRequest productItemImageUpdateRequest) {
    List<LocationPathAndCommonImage> locationPathAndCommonImageList = new ArrayList<>();
    Map<String, Boolean> locationPathAndCommonImageMap = new HashMap<>();
    for (ProductImage productImage : product.getProductImages()) {
      locationPathAndCommonImageMap.put(productImage.getLocationPath(), productImage.isCommonImage());
    }
    for (ProductItemImageRequest productItemImageRequest : productItemImageUpdateRequest.getNewProductItemImages()) {
      for (Image itemImage : productItemImageRequest.getItemImages()) {
        LocationPathAndCommonImage locationPathAndCommonImage = new LocationPathAndCommonImage();
        locationPathAndCommonImage.setLocationPath(itemImage.getLocationPath());
        locationPathAndCommonImage.setCommonImage(locationPathAndCommonImageMap.get(itemImage.getLocationPath()));
        locationPathAndCommonImageList.add(locationPathAndCommonImage);
      }
    }
    return locationPathAndCommonImageList;
  }

  public static void setActiveFlagInProductAndItemImages(Product product) {
    List<ProductImage> inactiveProductImages = new ArrayList<>();
    List<ProductItemImage> inactiveProductItemImages = new ArrayList<>();
    Map<String, Boolean> locationAndActiveFlagMap = new HashMap<>();

    if (CollectionUtils.isNotEmpty(product.getProductImages())) {
      inactiveProductImages =
          product.getProductImages().stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
              .filter(productImage -> !productImage.isActive())
              .filter(productImage -> StringUtils.isNotEmpty(productImage.getLocationPath()))
              .collect(Collectors.toList());
    }

    if (CollectionUtils.isNotEmpty(product.getProductItems())) {
      inactiveProductItemImages = product.getProductItems().stream()
          .filter(productItem -> CollectionUtils.isNotEmpty(productItem.getProductItemImages()))
          .flatMap(productItem -> productItem.getProductItemImages().stream())
          .filter(productItemImage -> !productItemImage.isMarkForDelete())
          .filter(productItemImage -> !productItemImage.isActive())
          .filter(productItemImage -> StringUtils.isNotEmpty(productItemImage.getLocationPath()))
          .collect(Collectors.toList());
    }

    Set<String> imagePaths = Stream.concat(inactiveProductImages.stream().map(ProductImage::getLocationPath),
        inactiveProductItemImages.stream().map(ProductItemImage::getLocationPath)).collect(Collectors.toSet());

    if (CollectionUtils.isNotEmpty(imagePaths)) {
      for (String imagePath : imagePaths) {
        boolean isActiveImageExist = fileStorageService.isFinalImageFileExist(imagePath);
        locationAndActiveFlagMap.put(imagePath, !isActiveImageExist);
      }

      inactiveProductImages.forEach(
          productImage -> productImage.setActive(locationAndActiveFlagMap.get(productImage.getLocationPath())));
      inactiveProductItemImages.forEach(productItemImage -> productItemImage.setActive(
          locationAndActiveFlagMap.get(productItemImage.getLocationPath())));
    }
  }

  public static String getImageFileName(String locationPath) {
    return StringUtils.substringBefore(StringUtils.substringAfterLast(locationPath, Constants.PATH_SEPARATOR),
        Constants.DOT);
  }

  public static void checkCommonImageMaxCount(boolean newProduct, boolean activated, boolean edited,
      boolean revisedActivated, boolean revisedUnactivated, Product product, Set<String> commonImageLocationPathSet) {
    int commonImageCount = 0;
    Map<String, List<ProductItemImage>> imagePathAndItemImageMap =
        product.getProductItems().stream().flatMap(productItem -> productItem.getProductItemImages().stream())
            .filter(productItemImage -> !productItemImage.isMarkForDelete()).collect(Collectors.groupingBy(
                productItemImage -> ConverterUtil.imageNameFromLocationPath(productItemImage.getLocationPath())));

    Map<String, List<ProductImage>> imagePathAndProductImageMap =
        product.getProductImages().stream().filter(productImage -> !productImage.isMarkForDelete()).collect(
            Collectors.groupingBy(
                productImage -> ConverterUtil.imageNameFromLocationPath(productImage.getLocationPath())));

    for (Map.Entry<String, List<ProductImage>> productImageEntry : imagePathAndProductImageMap.entrySet()) {
      if (isAnyImageCommonImageAndMfdFalse(productImageEntry.getValue())) {
        commonImageCount++;
        if (commonImageCount > MAX_COMMON_IMAGE) {
          for (ProductImage productImage : productImageEntry.getValue()) {
            productImage.setCommonImage(false);
            commonImageLocationPathSet.remove(productImage.getLocationPath());
          }
          List<ProductItemImage> productItemImageList = imagePathAndItemImageMap.get(productImageEntry.getKey());
          if (CollectionUtils.isNotEmpty(productItemImageList)) {
            productItemImageList.forEach(productItemImage -> productItemImage.setCommonImage(false));
          }
        }
      }
    }
  }

  private static boolean isAnyImageCommonImageAndMfdFalse(List<ProductImage> productImageList) {
    return productImageList.stream().anyMatch(productImage -> productImage.isCommonImage());
  }

  public static void resetProductImages(Product product, boolean regenerateProductImages) {
    if (regenerateProductImages) {
      //step 1 - soft delete product images which are not present in item images
      Set<String> imagesOnlyInDeletedVariant = getDeletedItemImageWhichIsNotPresentInAnyOtherItem(product);

      if (CollectionUtils.isNotEmpty(product.getProductImages())) {
        for (ProductImage productImage : product.getProductImages()) {
          if (!productImage.isMarkForDelete() && imagesOnlyInDeletedVariant.contains(
              productImage.getLocationPath())) {
            productImage.setMarkForDelete(true);
          }
        }
      }

      //step 2 - if there is no main image then set one image in product images as main image
      boolean isMainImagePresentAtProductLevelImages =
          Optional.ofNullable(product.getProductImages()).orElse(new ArrayList<>()).stream()
              .filter(Predicate.not(ProductImage::isMarkForDelete)).anyMatch(ProductImage::isMainImages);
      if (!isMainImagePresentAtProductLevelImages) {
        Map<String, List<ProductImage>> productImageMap =
            Optional.ofNullable(product.getProductImages()).orElse(new ArrayList<>()).stream()
                .filter(Predicate.not(ProductImage::isMarkForDelete))
                .collect(Collectors.groupingBy(productImage -> getImageName(productImage.getLocationPath())));
        Optional<String> getFirstVariantMainImage =
            Optional.ofNullable(product).map(Product::getProductItems).orElse(new ArrayList<>()).stream()
                .filter(Predicate.not(ProductItem::isMarkForDelete))
                .filter(productItem -> CollectionUtils.isNotEmpty(productItem.getProductItemImages())).findFirst()
                .map(ProductItem::getProductItemImages).orElse(new ArrayList<>()).stream()
                .filter(Predicate.not(ProductItemImage::isMarkForDelete)).filter(ProductItemImage::isMainImages)
                .map(ProductItemImage::getLocationPath).map(ProductImageUtil::getImageName).findFirst();
        if (getFirstVariantMainImage.isPresent() && productImageMap.containsKey(getFirstVariantMainImage.get())) {
          productImageMap.get(getFirstVariantMainImage.get()).forEach(productImage -> productImage.setMainImages(true));
        } else {
          Optional<Map.Entry<String, List<ProductImage>>> firstProductImage =
              productImageMap.entrySet().stream().findFirst();
          if (firstProductImage.isPresent()) {
            firstProductImage.get().getValue().stream().forEach(productImage -> productImage.setMainImages(true));
          }
        }
      }
    }
  }

  public static Set<String> getDeletedItemImageWhichIsNotPresentInAnyOtherItem(Product product) {
    Set<String> imagesOfDeletedItem =
        Optional.ofNullable(product).map(Product::getProductItems).orElse(new ArrayList<>()).stream()
            .filter(ProductItem::isMarkForDelete).map(ProductItem::getProductItemImages)
            .filter(CollectionUtils::isNotEmpty).flatMap(List::stream).map(ProductItemImage::getLocationPath)
            .collect(Collectors.toSet());

    Set<String> imagesOfNonDeletedItem =
        Optional.ofNullable(product).map(Product::getProductItems).orElse(new ArrayList<>()).stream()
            .filter(Predicate.not(ProductItem::isMarkForDelete)).map(ProductItem::getProductItemImages)
            .filter(CollectionUtils::isNotEmpty).flatMap(List::stream).map(ProductItemImage::getLocationPath)
            .collect(Collectors.toSet());

    imagesOfDeletedItem.removeAll(imagesOfNonDeletedItem);

    return imagesOfDeletedItem;
  }

  private static String getImageName(String imagePath) {
    String[] imageFilename = imagePath.split(File.separator);
    String imageName = FilenameUtils.removeExtension(imageFilename[imageFilename.length - 1]);
    return imageName;
  }

}
