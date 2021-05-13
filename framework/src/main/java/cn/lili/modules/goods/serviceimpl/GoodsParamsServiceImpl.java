package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CategoryParameterGroup;
import cn.lili.modules.goods.entity.dos.GoodsParams;
import cn.lili.modules.goods.entity.vos.GoodsParamsGroupVO;
import cn.lili.modules.goods.entity.vos.GoodsParamsVO;
import cn.lili.modules.goods.mapper.GoodsParamsMapper;
import cn.lili.modules.goods.service.CategoryParameterGroupService;
import cn.lili.modules.goods.service.GoodsParamsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * 商品关联参数接口实现
 *
 * @author pikachu
 * @version v1.0
 * @since v1.0
 * 2020-02-23 15:18:56
 */
@Service
@Transactional
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class GoodsParamsServiceImpl extends ServiceImpl<GoodsParamsMapper, GoodsParams> implements GoodsParamsService {
    //分类-参数绑定
    private final CategoryParameterGroupService categoryParameterGroupService;

    @Override
    public void addParams(List<GoodsParams> paramList, String goodsId) {
        //先删除现有商品参数
        this.remove(new UpdateWrapper<GoodsParams>().eq("goods_id", goodsId));
        //循环添加参数
        if (paramList != null) {
            for (GoodsParams param : paramList) {
                GoodsParams goodsParams = new GoodsParams();
                goodsParams.setGoodsId(goodsId);
                goodsParams.setParamName(param.getParamName());
                goodsParams.setParamValue(param.getParamValue());
                goodsParams.setParamId(param.getId());
                this.save(goodsParams);
            }
        }
    }

    @Override
    public List<GoodsParams> getGoodsParamsByGoodsId(String goodsId) {
        return this.list(new LambdaQueryWrapper<GoodsParams>().eq(GoodsParams::getGoodsId, goodsId));
    }

    /**
     * 添加商品参数
     *
     * @param goodsParamsVO 商品参数
     * @return 添加是否成功
     */
    @Override
    public boolean addParams(GoodsParamsVO goodsParamsVO) {
        return this.save(goodsParamsVO);
    }

    @Override
    public List<GoodsParamsVO> paramList(String goodsId, String categoryId) {
        return this.baseMapper.paramList(goodsId, categoryId);
    }

    @Override
    public List<GoodsParamsGroupVO> queryGoodsParams(String categoryId, String goodsId) {
        //查询分类关联参数组
        List<CategoryParameterGroup> groupList = categoryParameterGroupService.getCategoryGroup(categoryId);
        //查询商品参数
        List<GoodsParamsVO> paramList = this.paramList(goodsId, categoryId);
        //拼装数据返回
        return this.convertParamList(groupList, paramList);
    }


    /**
     * 拼装返回值
     *
     * @param paramList
     * @return
     */
    private List<GoodsParamsGroupVO> convertParamList(List<CategoryParameterGroup> groupList, List<GoodsParamsVO> paramList) {
        Map<String, List<GoodsParamsVO>> map = new HashMap<>(16);
        for (GoodsParamsVO param : paramList) {
            if (map.get(param.getGroupId()) != null) {
                map.get(param.getGroupId()).add(param);
            } else {
                List<GoodsParamsVO> list = new ArrayList<>();
                list.add(param);
                map.put(param.getGroupId(), list);
            }
        }
        List<GoodsParamsGroupVO> resList = new ArrayList<>();
        for (CategoryParameterGroup group : groupList) {
            GoodsParamsGroupVO list = new GoodsParamsGroupVO();
            list.setGroupName(group.getGroupName());
            list.setGroupId(group.getId());
            list.setParams(map.get(group.getId()));
            resList.add(list);
        }
        return resList;
    }
}