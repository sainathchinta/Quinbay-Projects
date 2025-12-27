package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.ActivateImageDTO;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductImageDTO;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.entity.ActivateImage;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductImageSingle;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.repository.ProductImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ImageServiceBean implements ImageService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ImageServiceBean.class);
  private static final int DEFAULT_PAGE_SIZE = 10;
  private static final int DEFAULT_PAGE_NUMBER = 0;

  @Autowired
  private ProductItemImageRepository productItemImageRepository;

  @Autowired
  private ProductImageRepository productImageRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductItemRepository productItemRepository;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  @Lazy
  private ProductItemService productItemService;

  @Autowired
  @Lazy
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Override
  @Transactional(readOnly = false)
  public void activateAndUpdateImageName(String storeId, String productCode, String locationPath, String hashCode) {
    this.productItemImageRepository.updateProductItemImageName(locationPath, hashCode);
    this.productImageRepository.updateProductImageName(locationPath, hashCode);
    Product product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductItemImagesCacheByStoreIdAndProductId(storeId, product.getId());
  }

  @Override
  @Transactional(readOnly = false)
  public ProductPublishUpdateDTO activateAndUpdateImagesName(String storeId, ProductActivateImageDTO dto, boolean skipReview)
      throws Exception {
    Map<String, LocationPathAndCommonImage> imagePathMap = new HashMap<>();
    Iterator<ActivateImageDTO> imageDTOIterator = dto.getImageRequests().iterator();
    while (imageDTOIterator.hasNext()) {
      ActivateImageDTO activateImageDTO = imageDTOIterator.next();
      imagePathMap.put(activateImageDTO.getHashCode(),
          new LocationPathAndCommonImage(activateImageDTO.getFilenames(), activateImageDTO.isCommonImage()));
    }
    List<ProductItemImage> productItemImages = new ArrayList<>();
    Product product =
        this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, dto.getProductCode());
    productService.setProductImagesCached(storeId, product, false);
    productItemServiceWrapper.setProductItemsWithProductItemImagesCached(storeId, product, false);
    List<ProductItemImage> finalItemImages = new ArrayList<>();
    Set<String> itemLevelImagesUpdatedItemSkuCodes = new HashSet<>();
    for (ProductItem productItem : product.getProductItems()) {
      boolean itemLevelImagesUpdated = false;
      if (CollectionUtils.isNotEmpty(productItem.getProductItemImages())) {
        for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
          if (skipReview) {
            ProductItemImage finalItemImage =
                new ProductItemImage(productItemImage.getProductItem(), productItemImage.isMainImages(),
                    imagePathMap.get(productItemImage.getHashCode()).getLocationPath(), productItemImage.getSequence(),
                    Boolean.TRUE, productItemImage.getHashCode());

            finalItemImage.setOriginalImage(false);
            finalItemImage.setRevised(false);
            finalItemImage.setProductItemId(productItemImage.getProductItem().getId());
            finalItemImage.setCommonImage(imagePathMap.get(productItemImage.getHashCode()).isCommonImage());
            finalItemImages.add(finalItemImage);
            itemLevelImagesUpdated = CommonUtil.itemLevelImagesUpdated(Arrays.asList(finalItemImage), itemLevelImagesUpdated);
            finalItemImages.add(productItemImage);
            productItemImage.setCommonImage(imagePathMap.get(productItemImage.getHashCode()).isCommonImage());
            productItemImage.setMarkForDelete(true);
          } else if (Objects.isNull(productItemImage.getOriginalImage()) || !productItemImage.getOriginalImage()) {
            if (imagePathMap.containsKey(productItemImage.getHashCode())) {
              productItemImage.setCommonImage(imagePathMap.get(productItemImage.getHashCode()).isCommonImage());
              if (!productItemImage.getLocationPath()
                  .equals(imagePathMap.get(productItemImage.getHashCode()).getLocationPath())
                  || !productItemImage.isActive()) {
                itemLevelImagesUpdated =
                    CommonUtil.itemLevelImagesUpdated(Arrays.asList(productItemImage), itemLevelImagesUpdated);
              }
              productItemImage.setActive(Boolean.TRUE);
              productItemImage.setLocationPath(imagePathMap.get(productItemImage.getHashCode()).getLocationPath());
              productItemImage.setProductItemId(productItemImage.getProductItem().getId());
            }
          }
        }
        productItemImages.addAll(productItem.getProductItemImages());
        if (itemLevelImagesUpdated) {
          itemLevelImagesUpdatedItemSkuCodes.add(productItem.getSkuCode());
        }
      }
    }
    if (skipReview) {
      this.productItemImageRepository.saveAll(finalItemImages);
      this.productItemImageRepository.flush();
    } else {
      if (CollectionUtils.isNotEmpty(productItemImages)) {
        this.productItemImageRepository.saveAll(productItemImages);
        this.productItemImageRepository.flush();
      }
    }
    List<ProductImage> finalImages = new ArrayList<>();
    List<ProductImage> productImages = product.getProductImages();
    log.info("updating active Images for productCode : {} and productImages : {} ", product.getProductCode(), productImages);
    boolean commonImageUpdated = false;
    if (CollectionUtils.isNotEmpty(productImages)) {
      for (ProductImage productImage : productImages) {
        if (skipReview) {
          ProductImage productImage1 =
              new ProductImage(product, productImage.isMainImages(), imagePathMap.get(productImage.getHashCode()).getLocationPath(),
                  productImage.getSequence(), productImage.getStoreId(), Boolean.TRUE, productImage.getHashCode());
          productImage1.setOriginalImage(false);
          productImage1.setRevised(false);
          productImage.setMarkForDelete(true);
          if (productImage.isCommonImage() != imagePathMap.get(productImage.getHashCode()).isCommonImage()) {
            productImage.setCommonImage(imagePathMap.get(productImage.getHashCode()).isCommonImage());
            commonImageUpdated =
                CommonUtil.commonImageUpdateWithProductImage(Arrays.asList(productImage), commonImageUpdated);
          }
          productImage1.setCommonImage(imagePathMap.get(productImage.getHashCode()).isCommonImage());
          finalImages.add(productImage);
          finalImages.add(productImage1);
        } else if (Objects.isNull(productImage.getOriginalImage()) || !productImage.getOriginalImage()) {
          if (imagePathMap.containsKey(productImage.getHashCode())) {
            productImage.setActive(Boolean.TRUE);
            productImage.setLocationPath(imagePathMap.get(productImage.getHashCode()).getLocationPath());
            if (productImage.isCommonImage() != imagePathMap.get(productImage.getHashCode()).isCommonImage()) {
              productImage.setCommonImage(imagePathMap.get(productImage.getHashCode()).isCommonImage());
              commonImageUpdated =
                  CommonUtil.commonImageUpdateWithProductImage(Arrays.asList(productImage), commonImageUpdated);
            }
          }
        }
      }
      if (skipReview) {
        log.info("updated product active images for productCode : {} and productImages : {}", product.getProductCode(),
            finalImages);
        this.productImageRepository.saveAll(finalImages);
        this.productImageRepository.flush();
      } else {
        log.info("updating product active images for productCode : {} and productImages : {}", product.getProductCode(),
            productImages);
        this.productImageRepository.saveAll(productImages);
        this.productImageRepository.flush();
      }
    }
    return new ProductPublishUpdateDTO(product, commonImageUpdated, itemLevelImagesUpdatedItemSkuCodes);
  }

  @Override
  public Set<String> findProductAndItemLocationPathsByProductCodeListAndStoreId(List<String> productCodeList,
      String storeId) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodeList),
        ErrorMessage.PRODUCT_CODE_LIST_MUST_NOT_BE_EMPTY.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
        ErrorMessage.STORE_ID_MUST_NOT_BE_EMPTY.getMessage());
    Set<String> productSet = new HashSet<>();
    productSet.addAll(
        this.productImageRepository.findProductLocationPathByProductCodeAndStoreId(productCodeList, storeId));
    productSet.addAll(
        this.productItemImageRepository.findProductItemImageLocationPathsByProductCodeListAndStoreId(productCodeList,
            storeId));
    return productSet;
  }

  @Override
  public ActivateImage isProductImagesActivated(String storeId, String productCode) throws Exception {
    Map<String, String> imageMap = new HashMap<String, String>();
    List<Object[]> productImages = this.productRepository.getAllProductImagesByProductCode(storeId, productCode);
    List<Object[]> productItemImages =
        this.productItemRepository.getAllProductItemImagesByProductCode(storeId, productCode);

    if (!CollectionUtils.isEmpty(productImages)) {
      for (Object[] productImagePath : productImages) {
        if ((Boolean) productImagePath[1]) {
          imageMap.put(productImagePath[0].toString(), productImagePath[0].toString());
        } else if ((Boolean) productImagePath[2]) {
          continue;
        } else {
          return new ActivateImage(productCode, false, new ArrayList<>());
        }
      }
    } else {
      LOGGER.error("Product images is empty for product code :{}", productCode);
      return new ActivateImage(productCode, false, new ArrayList<String>());
    }

    if (!CollectionUtils.isEmpty(productItemImages)) {
      for (Object[] productItemImagePath : productItemImages) {
        if ((Boolean) productItemImagePath[1]) {
          imageMap.put(productItemImagePath[0].toString(), productItemImagePath[0].toString());
        } else if ((Boolean) productItemImagePath[2]) {
          continue;
        } else {
          return new ActivateImage(productCode, false, new ArrayList<>());
        }
      }
    } else {
      LOGGER.error("Product item images is empty for product code :{}", productCode);
      return new ActivateImage(productCode, false, new ArrayList<String>());
    }

    List<String> filenames = new ArrayList<String>(imageMap.values());

    return new ActivateImage(productCode, true, filenames);
  }

  @Override
  @Transactional(readOnly = false)
  public ActivateImage updateImageName(String storeId, String productCode, String locationPath, String hashCode) {
    this.productItemImageRepository.updateProductItemImageName(locationPath, hashCode);
    this.productImageRepository.updateProductImageName(locationPath, hashCode);
    Product product =
        this.productService.getProductByStoreIdAndProductCodeAndMarkForDeleteFalseCached(storeId, productCode);
    productService.setCompleteProductDetailsCached(storeId, product, false);
    applicationCacheServiceBean.evictProductImagesCacheByStoreIdAndProductId(storeId, product.getId());
    applicationCacheServiceBean.evictProductItemImagesCacheByStoreIdAndProductId(storeId, product.getId());

    Map<String, String> imageMap = new HashMap<String, String>();
    try {
      List<Object[]> productImages = this.productRepository.getAllProductImagesByProductCode(storeId, productCode);
      List<Object[]> productItemImages =
          this.productItemRepository.getAllProductItemImagesByProductCode(storeId, productCode);

      if (!CollectionUtils.isEmpty(productImages)) {
        for (Object[] productImagePath : productImages) {
          if ((Boolean) productImagePath[1]) {
            imageMap.put(productImagePath[0].toString(), productImagePath[0].toString());
          } else if ((Boolean) productImagePath[2]) {
            continue;
          } else {
            throw new IllegalStateException("One of the product " + productCode + " images is not active!");
          }
        }
      } else {
        throw new IllegalStateException("Product " + productCode + " Images is empty!");
      }

      if (!CollectionUtils.isEmpty(productItemImages)) {
        for (Object[] productItemImagePath : productItemImages) {
          if ((Boolean) productItemImagePath[1]) {
            imageMap.put(productItemImagePath[0].toString(), productItemImagePath[0].toString());
          } else if ((Boolean) productItemImagePath[2]) {
            continue;
          } else {
            throw new IllegalStateException("One of the product items " + productCode + "  images is not active!");
          }
        }
      } else {
        throw new IllegalStateException("Product Item " + productCode + " Images is empty!");
      }

      List<String> filenames = new ArrayList<String>(imageMap.values());

      return new ActivateImage(productCode, true, filenames);
    } catch (Exception e) {
      LOGGER.error("Error updating image name {} {} ", productCode, locationPath, e);
      return new ActivateImage(productCode, false, new ArrayList<String>());
    }
  }

  @Override
  public List<ProductImageSingle> filterProductImagesByProductIds(List<String> productIds) {
    return this.productImageRepository.findProductImagesByProductIds(productIds);
  }

  @Override
  public List<ProductImageDTO> filterProductImagesByProductCodes(String storeId, List<String> productCodes) {
    final List<ProductImageDTO> result = new ArrayList<>();
    if (CollectionUtils.isEmpty(productCodes)) {
      return result;
    }
    Page<ProductImageDTO> pages = new PageImpl<ProductImageDTO>(Collections.emptyList());
    Pageable pageRequest = PageRequest.of(DEFAULT_PAGE_NUMBER, DEFAULT_PAGE_SIZE);
    do {
      pages = this.productImageRepository.findProductImagesByProductCodes(storeId, productCodes, pageRequest);
      result.addAll(pages.getContent());
      if (!pages.isLast()) {
        pageRequest = pages.nextPageable();
      }
    } while (pages.hasNext());
    return result;
  }

  @Override
  @Transactional(readOnly = true)
  @Cacheable(value = CacheNames.PRODUCT_IMAGES_CACHE, key = "#storeId +'_'+ #productId", unless = "#result == null")
  public List<ProductImage> getProductImagesByStoreIdAndProductIdCached(String storeId, String productId) {
    LOGGER.debug("ProductImage cache missed for storeId: {}, productId: {}", storeId, productId);
    List<ProductImage> productImages = productImageRepository.findByStoreIdAndProductId(storeId, productId);
    List<ProductImage> clonedProductImages = new ArrayList<>();
    for (ProductImage productImage : productImages) {
      ProductImage clonedProductImage = new ProductImage();
      BeanUtils.copyProperties(productImage, clonedProductImage, "product");
      clonedProductImages.add(clonedProductImage);
    }
    return clonedProductImages;
  }

  @Override
  public List<ProductImage> getProductImagesByStoreIdAndProductId(String storeId, String productId) {
    return productImageRepository.findByStoreIdAndProductId(storeId, productId);
  }

  @Override
  public List<ProductItemImage> getDetachedProductItemImagesByStoreIdAndProductItemIds(String storeId,
      List<String> productItemIds) {
    List<ProductItemImage> productItemImages =
        productItemImageRepository.findByStoreIdAndProductItemIdIn(storeId, productItemIds);
    List<ProductItemImage> clonedProductItemImages = new ArrayList<>();
    for (ProductItemImage productItemImage : productItemImages) {
      ProductItemImage clonedProductItemImage = new ProductItemImage();
      BeanUtils.copyProperties(productItemImage, clonedProductItemImage, "productItem");
      clonedProductItemImages.add(clonedProductItemImage);
    }
    return clonedProductItemImages;
  }

  @Override
  public void deleteProductImagesByIds(List<String> productImageIds) {
    productImageRepository.deleteAllById(productImageIds);
  }

  @Override
  public void deleteProductItemImagesByIds(List<String> productItemImageIds) {
    productItemImageRepository.deleteAllById(productItemImageIds);
  }
}
