const express=require('express');
const route=express.Router()

const services=require('../services/render');



route.get('/',services.homeRoutes);

app.get('/',(req,res)=>{
    res.render("index");
})

app.get('/add-task',(req,res)=>{
    res.render("add_task");
})

app.get('/update-task',(req,res)=>{
    res.render("update_task");
})

module.exports=route