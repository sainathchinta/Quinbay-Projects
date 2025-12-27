package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.Vendor;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.mta.distributiontask.model.VendorQuotaCounter;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface VendorQuotaCounterRepository extends JpaRepository<VendorQuotaCounter,String> {

    @Modifying
    @Query("update VendorQuotaCounter vq set vq.totalReviewInProgress = (vq.totalReviewInProgress + :count) where vq.vendor = :vendor")
    void incrementInProgressQuota(@Param("vendor") Vendor vendor, @Param("count") Integer count);

    @Modifying
    @Query("update VendorQuotaCounter vq set vq.totalReviewInProgress = (vq.totalReviewInProgress - :count) where vq.vendor = :vendor")
    void decrementInProgressQuota(@Param("vendor") Vendor vendor, @Param("count") Integer count);
}
