package com.gdn.x.productcategorybase.repository.brand;

import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Date;
import java.util.List;

public interface BrandAuthorisationWipRepository
    extends JpaRepository<BrandAuthorisationWip, String>, BrandAuthorisationWipRepositoryCustom {

  /**
   *
   * @param storeId non null store id
   * @param brandCode non null brand code
   * @param sellerCode non null seller code
   * @param markForDelete markForDelete
   * @return List of Brand Auth Wip Entity
   */
  List<BrandAuthorisationWip> findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(String storeId,
    String brandCode, String sellerCode, boolean markForDelete);

  /**
   *
   * @param storeId non null store id
   * @param brandCode non null brand code
   * @param sellerCode non null seller code
   * @return Brand Auth Wip Entity
   */
  BrandAuthorisationWip findByStoreIdAndBrandCodeAndSellerCode(String storeId, String brandCode,
    String sellerCode);

  /**
   *
   * @param storeId non null store id
   * @param brandCode non null brand code
   * @param sellerCode non null seller code
   * @param brandAuthorizationWipStatus non null status
   * @return Brand Auth Wip Entity
   */
  BrandAuthorisationWip findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(String storeId,
      String brandCode, String sellerCode, BrandAuthorizationWipStatus brandAuthorizationWipStatus);

  /**
   * return the count of pending brand-auth requests
   *
   * @param sellerCode
   * @param brandAuthorizationWipStatuses
   * @return
   */
  long countBySellerCodeAndAuthorisationStatusIn(String sellerCode,
      List<BrandAuthorizationWipStatus> brandAuthorizationWipStatuses);

  /**
   * Fetch Upcoming brand auth status for next day
   *
   * @param storeId                Store id
   * @param authorizationWipStatus Brand Auth Wip Status
   * @param authStartDateAfter     Auth Start Date after
   * @param authStartDateBefore    Auth Start Date before
   * @return List of brand authorisation wip
   */
  List<BrandAuthorisationWip> findByStoreIdAndAuthorisationStatusAndAuthStartDateBetween(
    String storeId, BrandAuthorizationWipStatus authorizationWipStatus, Date authStartDateAfter,
    Date authStartDateBefore);

  /**
   * Fetch on the basis of brand code and seller code and status
   * @param brandCode Brand code
   * @param sellerCode Seller code
   * @param brandAuthorizationWipStatus Brand Authorisation Status
   * @return Return BrandAuthorisationWip
   */
  BrandAuthorisationWip findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
    String brandCode, String sellerCode, BrandAuthorizationWipStatus brandAuthorizationWipStatus);

  @Modifying
  @Query("UPDATE BrandAuthorisationWip brandAuthorisationWip SET "
    + "brandAuthorisationWip.markForDelete = true, "
    + "brandAuthorisationWip.updatedDate = CURRENT_TIMESTAMP, "
    + "brandAuthorisationWip.updatedBy = :username "
    + "WHERE brandAuthorisationWip.id = :id")
  void deleteUpcomingBrandAuthById(@Param("id") String id, @Param("username") String username);

}