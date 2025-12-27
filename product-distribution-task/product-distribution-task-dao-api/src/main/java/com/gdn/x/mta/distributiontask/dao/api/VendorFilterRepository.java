package com.gdn.x.mta.distributiontask.dao.api;

import com.gdn.x.mta.distributiontask.model.VendorDefaultFilter;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface VendorFilterRepository extends JpaRepository<VendorDefaultFilter,String> {

    VendorDefaultFilter findByStoreIdAndVendorEmailAndMarkForDeleteFalse(String storeId, String vendorEmail);
}
