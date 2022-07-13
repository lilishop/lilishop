package cn.lili.modules.page.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.SystemSettingProperties;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.page.entity.dos.PageData;
import cn.lili.modules.page.entity.dto.PageDataDTO;
import cn.lili.modules.page.entity.enums.PageEnum;
import cn.lili.modules.page.entity.vos.PageDataListVO;
import cn.lili.modules.page.entity.vos.PageDataVO;
import cn.lili.modules.page.mapper.PageDataMapper;
import cn.lili.modules.page.service.PageDataService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 楼层装修管理业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/11 9:15
 */
@Service
public class PageDataServiceImpl extends ServiceImpl<PageDataMapper, PageData> implements PageDataService {


    @Autowired
    private SystemSettingProperties systemSettingProperties;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void addStorePageData(String storeId) {
        //设置店铺的PC页面
        PageData pageData = new PageData();
        pageData.setNum(storeId);
        pageData.setPageClientType(ClientTypeEnum.PC.value());
        pageData.setPageShow(SwitchEnum.OPEN.name());
        pageData.setPageType(PageEnum.STORE.value());
        this.save(pageData);

        //设置店铺的Mobile页面
        PageData mobilePageData = new PageData();
        mobilePageData.setNum(storeId);
        mobilePageData.setPageClientType(ClientTypeEnum.H5.value());
        mobilePageData.setPageShow(SwitchEnum.OPEN.name());
        mobilePageData.setPageType(PageEnum.STORE.value());
        this.save(mobilePageData);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PageData addPageData(PageData pageData) {
        //如果页面为发布，则关闭其他页面，开启此页面
        //演示站点不可以开启楼层
        if (!Boolean.TRUE.equals(systemSettingProperties.getIsDemoSite()) && pageData.getPageShow().equals(SwitchEnum.OPEN.name())) {
            LambdaUpdateWrapper<PageData> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.eq(PageData::getPageType, pageData.getPageType());
            lambdaUpdateWrapper.eq(PageData::getPageClientType, pageData.getPageClientType());
            lambdaUpdateWrapper.set(PageData::getPageShow, SwitchEnum.CLOSE.name());
            this.update(lambdaUpdateWrapper);
        } else {
            pageData.setPageShow(SwitchEnum.CLOSE.name());
        }
        this.save(pageData);
        return pageData;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PageData updatePageData(PageData pageData) {
        //如果页面为发布，则关闭其他页面，开启此页面
        if (pageData.getPageShow() != null && pageData.getPageShow().equals(SwitchEnum.OPEN.name())) {
            LambdaUpdateWrapper<PageData> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.eq(CharSequenceUtil.isNotEmpty(pageData.getPageType()), PageData::getPageType, pageData.getPageType());
            lambdaUpdateWrapper.eq(CharSequenceUtil.isNotEmpty(pageData.getPageClientType()), PageData::getPageClientType, pageData.getPageClientType());
            lambdaUpdateWrapper.set(PageData::getPageShow, SwitchEnum.CLOSE.name());
            this.update(lambdaUpdateWrapper);
        } else {
            pageData.setPageShow(SwitchEnum.CLOSE.name());
        }
        LambdaUpdateWrapper<PageData> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.set(PageData::getPageData, pageData.getPageData());
        lambdaUpdateWrapper.eq(PageData::getId, pageData.getId());
        this.updateById(pageData);
        return pageData;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public PageData releasePageData(String id) {
        PageData pageData = this.getById(id);


        //如果已经发布，不能重复发布
        if (pageData.getPageShow().equals(SwitchEnum.OPEN.name())) {
            throw new ServiceException(ResultCode.PAGE_RELEASE_ERROR);
        }

        LambdaUpdateWrapper<PageData> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.set(PageData::getPageShow, SwitchEnum.CLOSE.name());
        lambdaUpdateWrapper.eq(PageData::getPageType, pageData.getPageType());
        lambdaUpdateWrapper.eq(PageData::getPageClientType, pageData.getPageClientType());
        //如果是店铺需要设置店铺ID
        if (pageData.getPageType().equals(PageEnum.STORE.value())) {
            lambdaUpdateWrapper.eq(PageData::getNum, pageData.getNum());
        }
        //设置禁用所有店铺首页
        this.update(lambdaUpdateWrapper);

        //将当前页面启用
        LambdaUpdateWrapper<PageData> wrapper = Wrappers.lambdaUpdate();
        wrapper.set(PageData::getPageShow, SwitchEnum.OPEN.name());
        wrapper.eq(PageData::getId, pageData.getId());
        this.update(wrapper);
        return pageData;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean removePageData(String id) {
        PageData pageData = this.getById(id);
        //专题则直接进行删除
        if (pageData.getPageType().equals(PageEnum.SPECIAL.name())) {
            return this.removeById(id);
        }
        //店铺、平台首页需要判断是否开启，开启则无法删除
        if (pageData.getPageShow().equals(SwitchEnum.OPEN.name())) {
            throw new ServiceException(ResultCode.PAGE_OPEN_DELETE_ERROR);
        }
        //判断是否有其他页面，如果没有其他的页面则无法进行删除
        QueryWrapper<Integer> queryWrapper = Wrappers.query();
        queryWrapper.eq(pageData.getPageType() != null, "page_type", pageData.getPageType());
        queryWrapper.eq(pageData.getPageClientType() != null, "page_client_type", pageData.getPageClientType());
        //如果为店铺页面需要设置店铺ID
        if (pageData.getPageType().equals(PageEnum.STORE.name())) {
            queryWrapper.eq(pageData.getNum() != null, "num", pageData.getNum());
        }
        //判断是否为唯一的页面
        if (this.baseMapper.getPageDataNum(queryWrapper) == 1) {
            throw new ServiceException(ResultCode.PAGE_DELETE_ERROR);
        }
        return this.removeById(id);
    }

    @Override
    public PageDataVO getPageData(PageDataDTO pageDataDTO) {

        //如果获取的是专题、店铺页面数据需要传入ID
        if (!pageDataDTO.getPageType().equals(PageEnum.INDEX.name()) && pageDataDTO.getNum() == null) {
            throw new ServiceException(ResultCode.PAGE_NOT_EXIST);
        }
        QueryWrapper<PageDataVO> queryWrapper = Wrappers.query();
        queryWrapper.eq(pageDataDTO.getPageType() != null, "page_type", pageDataDTO.getPageType());
        queryWrapper.eq(pageDataDTO.getNum() != null, "num", pageDataDTO.getNum());
        queryWrapper.eq("page_show", SwitchEnum.OPEN.name());

        queryWrapper.eq("page_client_type", pageDataDTO.getPageClientType());

        return this.baseMapper.getPageData(queryWrapper);
    }

    @Override
    public IPage<PageDataListVO> getPageDataList(PageVO pageVO, PageDataDTO pageDataDTO) {
        QueryWrapper<PageDataListVO> queryWrapper = Wrappers.query();
        queryWrapper.eq(pageDataDTO.getPageType() != null, "page_type", pageDataDTO.getPageType());
        queryWrapper.eq(pageDataDTO.getNum() != null, "num", pageDataDTO.getNum());
        queryWrapper.eq(pageDataDTO.getPageClientType() != null, "page_client_type", pageDataDTO.getPageClientType());

        return this.baseMapper.getPageDataList(PageUtil.initPage(pageVO), queryWrapper);

    }
}